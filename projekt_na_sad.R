#!/usr/bin/env Rscript
options(repos = c(CRAN = "https://cran.r-project.org"))
setwd("C:/Users/Administrator/OneDrive/Pulpit/projekt_na_sad")

install.packages("Hmisc")
library(Hmisc)
install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("car")
library(car)
library(gridExtra)
install.packages("stats")
library(stats)
install.packages("dunn.test")
library(dunn.test)
install.packages("FSA")
library(FSA)
library(ggplot2)


args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat("Brak podanego pliku CSV jako argument.")
} else {
  dane <- read.csv2(args[1], sep = ";")  # Pierwszy argument to nazwa pliku CSV
  # Wczytaj dane z pliku_csv i przetwarzaj je dalej
}

#dane <- read.csv2("przykladoweDane-Projekt.csv", sep = ";")

################### PUNKT 1 ########################

# sprawdzanie, czy nie ma braków w danych

is.na(dane)
sum(is.na(dane))
which(is.na(dane))

# zastapienie brakujacych danych

fill_na_with_col_mean <- function(df) {
  for (col in names(df)) { # iteracja po kolumnach
    if (is.numeric(df[[col]])) { # sprawdzenie, czy kolumna zawiera liczby
      col_mean <- mean(df[[col]], na.rm = TRUE) # wyliczenie średniej dla kolumny
      if (any(is.na(df[[col]]))) { # sprawdzenie, czy kolumna zawiera NA
        na_indices <- which(is.na(df[[col]])) # indeksy NA w kolumnie
        for (i in na_indices) { # iteracja po indeksach NA
          df[i, col] <- col_mean # zastąpienie NA średnią kolumny
          message(paste("Wiersz", i, "kolumna", col, "została zmieniona z NA na", col_mean))
        }
      }
    }
  }
  return(df)
}

poprawione_dane <- fill_na_with_col_mean(dane)

# informacja o wartościach odstających

# a) kolumny z wartościami liczbowymi
num_cols <- sapply(poprawione_dane, is.numeric)

# b) iteracja po kolumnach z wartościami liczbowymi
for (col in names(poprawione_dane[num_cols])) {
  # kwantyle
  q1 <- quantile(poprawione_dane[[col]], 0.25, na.rm = TRUE)
  q3 <- quantile(poprawione_dane[[col]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  # wyznaczanie wartości odstających
  outliers <- poprawione_dane[[col]] < (q1 - 1.5 * iqr) | poprawione_dane[[col]] > (q3 + 1.5 * iqr)
  
  # wyświetlanie
  cat("Kolumna:", col, "\n")
  cat("Liczba wartości odstających:", sum(outliers), "\n")
  cat("Wartości odstające:", paste(poprawione_dane[outliers, col], collapse = ", "), "\n\n")
}


############################### PUNKT 2 ##########################

# Inicjalizacja pustej listy na tabele dla poszczególnych grup
list_of_tables <- list()
poprawione_dane_split <- split(poprawione_dane, poprawione_dane$grupa)

for (group_value in unique(poprawione_dane$grupa)) {
  dane_grupy <- poprawione_dane_split[[group_value]]
  num_cols <- sapply(dane_grupy, is.numeric)
  
  tabela <- data.frame(Grupa = character(),
                       Kolumna = character(),
                       Zakres = character(),
                       Srednia = character(),
                       Mediana = character(),
                       IQR = character(),
                       Wariancja = character(),
                       OdchylenieStd = character(),
                       stringsAsFactors = FALSE)
  
  for (col in names(dane_grupy)[num_cols]) {
    range_values <- range(dane_grupy[[col]], na.rm = TRUE)
    mean_value <- mean(dane_grupy[[col]], na.rm = TRUE)
    median_value <- median(dane_grupy[[col]], na.rm = TRUE)
    iqr_value <- IQR(dane_grupy[[col]], na.rm = TRUE)
    var_value <- var(dane_grupy[[col]], na.rm = TRUE)
    sd_value <- sd(dane_grupy[[col]], na.rm = TRUE)
    
    tabela <- rbind(tabela, 
                    data.frame(Grupa = group_value,
                               Kolumna = col,
                               Zakres = paste(round(range_values, 3), collapse = "-"),
                               Srednia = formatC(mean_value, digits = 3, format = "f"),
                               Mediana = formatC(median_value, digits = 3, format = "f"),
                               IQR = formatC(iqr_value, digits = 3, format = "f"),
                               Wariancja = formatC(var_value, digits = 3, format = "f"),
                               OdchylenieStd = formatC(sd_value, digits = 3, format = "f"),
                               stringsAsFactors = FALSE))
  }
  
  tabela$Zakres <- formatC(tabela$Zakres, width = 12)
  tabela$Srednia <- formatC(tabela$Srednia, width = 12)
  tabela$Mediana <- formatC(tabela$Mediana, width = 12)
  tabela$IQR <- formatC(tabela$IQR, width = 12)
  tabela$Wariancja <- formatC(tabela$Wariancja, width = 12)
  tabela$OdchylenieStd <- formatC(tabela$OdchylenieStd, width = 12)
  
  # Dodanie tabeli do listy, z indeksem opartym na wartości grupy
  list_of_tables[[as.character(group_value)]] <- tabela
}

# Zapisanie tabel do plików dla poszczególnych grup
for (group_table in list_of_tables) {
  group_value <- group_table$Grupa[1] # Pobranie wartości grupy
  write.table(group_table, paste0("tabela_", group_value, ".txt"), sep = "\t", row.names = FALSE)
}


######################### PUNKT 3 ###############################

######################### rozkład normalny ######################

### osobne wykresy ###



# Inicjalizacja pustej listy na wyniki testu Shapiro-Wilka
wyniki_shapiro <- list()

for (group_value in unique(poprawione_dane$grupa)) {
  cat("Rozkład normalny dla GRUPA =", group_value, ":\n")
  dane_grupy <- poprawione_dane_split[[group_value]]
  num_cols <- sapply(dane_grupy, is.numeric)
  dane_numeryczne <- dane_grupy[, num_cols]
  normalne_parametry <- c()
  
  # Inicjalizacja pustej tabeli na wyniki testu Shapiro-Wilka dla danej grupy
  wyniki_shapiro_grupa <- data.frame(Kolumna = character(), PValue = numeric())
  
  for (col in names(dane_numeryczne)) {
    if (length(unique(dane_numeryczne[[col]])) <= 2) {
      cat("Kolumna:", col, "zbyt mało unikalnych wartości, test normalności nie ma sensu.\n")
    } else {
      shapiro_test <- shapiro.test(dane_numeryczne[[col]])
      cat("Kolumna:", col, "p-value testu Shapiro-Wilka:", shapiro_test$p.value, "\n")  
      
      # Dodaj wynik testu Shapiro-Wilka do tabeli dla danej grupy
      wyniki_shapiro_grupa <- rbind(wyniki_shapiro_grupa, data.frame(Kolumna = col, PValue = shapiro_test$p.value))
      
      if (shapiro_test$p.value > 0.05) {
        ggplot(dane_grupy, aes_string(x = col)) + 
          geom_density() + 
          ggtitle(paste("Wykres gęstości dla kolumny", col, "dla grupy", group_value)) + 
          theme(plot.title = element_text(hjust = 0.5)) + 
          facet_wrap(~grupa)
        ggsave(filename = paste("wykres_", group_value, "_", col, ".pdf", sep = ""), 
               plot = last_plot())
        normalne_parametry <- c(normalne_parametry, col)
      } else {
        cat("Kolumna:", col, "nie ma rozkładu normalnego.\n")
      }
    }
  }
  
  wyniki_shapiro[[group_value]] <- wyniki_shapiro_grupa
  
  if (nrow(wyniki_shapiro_grupa) > 0) {
    file_name <- paste("parametry_normalne_", group_value, ".txt", sep = "")
    write.table(wyniki_shapiro_grupa, file_name, quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
  }
  
  cat("\n")
}




## wykres laczony ##

for (col in names(dane_numeryczne)) {
  gg_list <- list()
  
  for (group_value in unique(poprawione_dane$grupa)) {
    dane_grupy <- poprawione_dane_split[[group_value]]
    gg <- ggplot(dane_grupy, aes(x = .data[[col]], fill = grupa)) +
      geom_density(alpha = 0.5) +
      ggtitle(paste(group_value)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = col, y = "Gęstość") +
      theme(legend.position = "none")
    
    gg_list[[group_value]] <- gg
    
    # Wykonanie testu Shapiro-Wilk'a na danych grupy
    shapiro_test <- shapiro.test(dane_grupy[[col]])
    
    if (shapiro_test$p.value > 0.05) {
      cat("Dla kolumny", col, "i grupy", group_value, "dane mają rozkład normalny (p-value =", shapiro_test$p.value, ")\n")
    } else {
      cat("Dla kolumny", col, "i grupy", group_value, "dane nie mają rozkładu normalnego (p-value =", shapiro_test$p.value, ")\n")
    }
  }
  
  combined_gg <- do.call(grid.arrange, c(gg_list, ncol = 3))
  file_name <- paste("wykres_gestosci_", col, ".pdf", sep = "")
  ggsave(filename = file_name, plot = combined_gg)
  cat("Zapisano wykres:", file_name, "\n")
}

##

#################### ocena homogeniczności wariancji ########################

lista_numeryczne <- list()

# Pobierz unikalne grupy
unikalne_grupy <- unique(poprawione_dane$grupa)

# Iteruj przez każdą unikalną grupę i twórz listę dane_grupy
for (group_value in unikalne_grupy) {
  dane_grupy <- poprawione_dane[poprawione_dane$grupa == group_value, ]
  lista_numeryczne[[group_value]] <- dane_grupy
}

# Wybierz kolumny do testu Levene'a (wyklucz pierwsze dwie kolumny)
kolumny_do_testu <- names(poprawione_dane)[-c(1, 2)] 

# Inicjalizacja pustej listy na wyniki testów Levene'a
wyniki_levene <- list()

przeprowadzTestLevene <- function(grupa, kolumna) {
  wynik_testu <- car::leveneTest(get(kolumna) ~ grupa, data = dane)
  return(wynik_testu)
}

levene_lepsza <- list()

for (kolumna in kolumny_do_testu) {
  wyniki_dla_kolumny <- list()
  
  # Iteruj po grupach
  for (grupa in unique(dane$grupa)) {
    wynik <- przeprowadzTestLevene(grupa, kolumna)
    wyniki_dla_kolumny[[grupa]] <- wynik
    levene_lepsza[[grupa]] <- wynik
  }
  
  # Przypisz wyniki dla danej kolumny do wyniki_levene
  wyniki_levene[[kolumna]] <- wyniki_dla_kolumny
}


############### porównywanie grup (porównywanie średnich)  #####################

p_value_shapiro <- list()
for (grupa in names(wyniki_shapiro)) {
  p_value_shapiro[[grupa]] <- list()
  for (kolumna in names(wyniki_shapiro[[grupa]])) {
    p_value_shapiro[[grupa]][[kolumna]] <- wyniki_shapiro[[grupa]][[kolumna]]
  }
}

# Wydobywanie p-value z wyników testów Levene'a
p_value_levene <- list()
for (kolumna in names(wyniki_levene)) {
  p_value_levene[[kolumna]] <- list()
  for (grupa in names(wyniki_levene[[kolumna]])) {
    p_value_levene[[kolumna]][[grupa]] <- wyniki_levene[[kolumna]][[grupa]]$Pr
  }
}

# Teraz masz dostęp do p-value dla każdej kolumny i grupy w obu wynikach

# Inicjalizacja listy wyników testu Kruskala-Wallisa

wyniki_kruskala <- list()
wyniki_anova <- list()
wyniki_dunna <- list()
wyniki_tukeya <- list()
wyniki_tstudenta <- list()
wyniki_welcha <- list()
wyniki_wilcoxa <- list()

# Wybieramy tylko kolumny numeryczne
kolumny_do_testu <- sapply(poprawione_dane, is.numeric)


# Teraz wyniki testów są przechowywane w wyniki_kruskala, gdzie klucze to nazwy kolumn numerycznych


for (kolumna in names(poprawione_dane)[kolumny_do_testu]) {
  p_value_shapiro <- shapiro.test(poprawione_dane[[kolumna]])$p.value
  p_value_levene <- leveneTest(poprawione_dane[[kolumna]] ~ grupa, data = poprawione_dane)$Pr
  p_value_levene1 <- na.omit(p_value_levene)
  
  if(length(unikalne_grupy) > 2) {
    
  # Sprawdzamy, czy wartości p-value spełniają warunek
  if (p_value_shapiro > 0.05 && p_value_levene1 > 0.05) {
    wyniki_anova[[kolumna]] <- summary(aov(as.formula(paste(kolumna, "~ grupa")), data = poprawione_dane))
    
    if (!is.null(wyniki_anova[[kolumna]])) {
      p_value <- wyniki_anova[[kolumna]][[1]]$`Pr(>F)`
      p_value1 <- na.omit(p_value)
      
      if (!is.null(p_value1) && p_value1 <= 0.05) {
        cat("\n\n", kolumna, " ", p_value1, "<= 0.05 - są różnice pomiędzy grupami\n")
        cat("\n\nTest Tukeya dla: ", kolumna, "\n")
        formula <- as.formula(paste(kolumna, "~ grupa"))
        anova_result <- aov(formula, data = poprawione_dane)
        test_tukeya <- TukeyHSD(anova_result)
        wyniki_tukeya[[kolumna]] <- test_tukeya
        print(wyniki_tukeya[[kolumna]])
      } else {
        cat("\n\n", kolumna, " ", p_value1, "> 0.05 - brak różnic pomiędzy grupami\n")
      }
    }
    
  } else {
    wyniki_kruskala[[kolumna]] <- kruskal.test(poprawione_dane[[kolumna]] ~ grupa, data = poprawione_dane)
    
    if(wyniki_kruskala[[kolumna]]$p.value <= 0.05){
      cat("\n\n", kolumna, " ", wyniki_kruskala[[kolumna]]$p.value, "<= 0.05 - są różnice pomiędzy grupami\n")
      cat("\n\nTest Dunna dla: ", kolumna, "\n")
      test_dunna <- dunn.test(poprawione_dane[[kolumna]], g = poprawione_dane$grupa)
      wyniki_dunna[[kolumna]] <- test_dunna
      
    }else{
      cat("\n\n", kolumna, " ", wyniki_kruskala[[kolumna]]$p.value, "> 0.05 - brak różnic pomiędzy grupami\n")
    }
    
    
  }
  }
  else if(length(unikalne_grupy) == 2) {
    if (p_value_shapiro > 0.05 && p_value_levene1 > 0.05) {
      cat("\nT-STUDENT")
      formula_t <- as.formula(paste(kolumna, "~ grupa"))
      wyniki_tstudenta[kolumna] <- t.test(formula_t, data = dwie_grupy)
      
      if (!is.null(wyniki_tstudenta[kolumna])) {
        p_value <- wyniki_tstudenta[[kolumna]]
        p_value1 <- na.omit(p_value)
        
        if (!is.null(p_value1) && p_value1 <= 0.05) {
          cat("\n\n", kolumna, " ", p_value1, "<= 0.05 - są różnice pomiędzy grupami\n")
        } else {
          cat("\n\n", kolumna, " ", p_value1, "> 0.05 - brak różnic pomiędzy grupami\n")
        }
      }
    }
    else if (p_value_shapiro > 0.05 && p_value_levene1 <= 0.05) {
      cat("\nWELCH")
      formula_welch <- as.formula(paste(kolumna, "~ grupa"))
      wyniki_welcha <- t.test(formula_welch, data = dwie_grupy, var.equal = FALSE)
      
    
      if (!is.null(wyniki_welcha[[kolumna]]$p.value)) {
        p_value <- wyniki_welcha[[kolumna]]$p.value
        p_value1 <- as.numeric(p_value)
        
        if (!is.null(p_value1) && p_value1 <= 0.05) {
          cat("\n\n", kolumna, " ", p_value1, "<= 0.05 - są różnice pomiędzy grupami\n")
        } else {
          cat("\n\n", kolumna, " ", p_value1, "> 0.05 - brak różnic pomiędzy grupami\n")
        }
      }
    }
    else if (p_value_shapiro <= 0.05) {
      cat("\nWILCOX")
      formula_wilch <- as.formula(paste(kolumna, "~ grupa"))
      wyniki_wilcoxa[[kolumna]] <- wilcox.test(poprawione_dane[[kolumna]] ~ grupa, data = poprawione_dane)
      
      if (!is.null(wyniki_wilcoxa[[kolumna]]$p.value)) {
        p_value <- wyniki_wilcoxa[[kolumna]]$p.value
        p_value1 <- as.numeric(p_value)
    
    if (!is.null(p_value1) && p_value1 <= 0.05) {
      cat("\n\n", kolumna, " ", p_value1, "<= 0.05 - są różnice pomiędzy grupami\n")
    } else {
      cat("\n\n", kolumna, " ", p_value1, "> 0.05 - brak różnic pomiędzy grupami\n")
    }
  }
}

    
  }
}



################# dane jakościowe ##############################################

table(poprawione_dane$grupa, poprawione_dane$plec)

chisq.test(dane$grupa, dane$plec)
pvalueChisqPlec <- chisq.test(poprawione_dane$grupa, poprawione_dane$plec)$p.value
pvalueChisqPlec

# Otwórz plik PNG
png("wykres_plci.pdf", width = 800, height = 600)

# Tworzenie wykresu barplot
barplot(table(poprawione_dane$plec, poprawione_dane$grupa),
        ylim = c(0, 22),
        beside = TRUE,
        col = c("#ffb3b3", "#b3d1ff"),
        xlab = "grupa",
        ylab = "płeć",
        legend = c("kobieta", "mężczyzna")
)

# Dodaj tekst z p-value
text(1.5, 20, paste("p-value", round(pvalueChisqPlec, digits = 3)))



# Zamknij plik graficzny
dev.off()

###################################### PUNKT 4 ###################################

######################### testy korelacji #######################################

# Tworzymy wektor z nazwami kolumn numerycznych
kolumny_do_testu <- names(poprawione_dane)[unlist(lapply(poprawione_dane, is.numeric))]

# Puste listy na wyniki
wyniki_korelacji_spearman <- list()
wyniki_korelacji_pearson <- list()

# Funkcja do opisania siły i kierunku korelacji
opisz_korelacje <- function(korelacja) {
  if (abs(korelacja) >= 0.7) {
    sila <- "silna"
  } else if (abs(korelacja) >= 0.4) {
    sila <- "umiarkowana"
  } else {
    sila <- "słaba"
  }
  
  if (korelacja > 0) {
    kierunek <- "pozytywny"
  } else if (korelacja < 0) {
    kierunek <- "negatywny"
  } else {
    kierunek <- "brak"
  }
  
  opis <- paste("Korelacja jest", sila, "(", abs(korelacja), ") i ma", kierunek, "kierunek.")
  return(opis)
}

# Lista z nazwami grup
grupy <- unique(poprawione_dane$grupa)

# Pętla dla każdej grupy
for (g in grupy) {
  cat("\n\n\nGRUPA: ", g, "\n\n")
  # Filtrujemy dane tylko dla danej grupy
  dane_grupa <- poprawione_dane %>% filter(grupa == g)
  
  # Pętla dla każdej pary kolumn numerycznych
  for (i in 1:length(kolumny_do_testu)) {
    for (j in 1:length(kolumny_do_testu)) {
      if (i != j) {
        # Przeprowadzamy test korelacji Spearmana
        wynikTestuKorelacjiSpearmana <- cor.test(dane_grupa[[kolumny_do_testu[i]]], dane_grupa[[kolumny_do_testu[j]]], method = "spearman")
        
        # Zapisujemy wyniki
        wyniki_korelacji_spearman[[paste(grupa, kolumny_do_testu[i], kolumny_do_testu[j])]] <- wynikTestuKorelacjiSpearmana
        
        # Opisujemy wyniki
        opis_spearman <- opisz_korelacje(wynikTestuKorelacjiSpearmana$estimate)
        cat(paste("Korelacja Spearmana między", kolumny_do_testu[i], "a", kolumny_do_testu[j], "dla grupy", grupa, ":", opis_spearman), "\n")
        
        # Przeprowadzamy test korelacji Pearsona
        wynikTestuKorelacjiPearsona <- cor.test(dane_grupa[[kolumny_do_testu[i]]], dane_grupa[[kolumny_do_testu[j]]], method = "pearson")
        
        # Zapisujemy wyniki
        wyniki_korelacji_pearson[[paste(grupa, kolumny_do_testu[i], kolumny_do_testu[j])]] <- wynikTestuKorelacjiPearsona
        
        # Opisujemy wyniki
        opis_pearson <- opisz_korelacje(wynikTestuKorelacjiPearsona$estimate)
        cat(paste("Korelacja Pearsona między", kolumny_do_testu[i], "a", kolumny_do_testu[j], "dla grupy", grupa, ":", opis_pearson), "\n")
      }
    }
  }
}



###################### wykres korelacji ########################################

kolumny_do_testu <- names(poprawione_dane)[-c(1, 2)] 
# Tworzymy wektor z unikalnymi grupami
grupy <- unique(poprawione_dane$grupa)


# Pętla dla każdej grupy
for (grupa in grupy) {
  
  # Filtrujemy dane dla danej grupy
  dane_grupa <- poprawione_dane %>% filter(grupa == grupa)
  
  # Pętla dla każdej pierwszej kolumny numerycznej
  for (kolumna1 in kolumny_do_testu) {
    # Pętla dla każdej drugiej kolumny numerycznej
    for (kolumna2 in kolumny_do_testu) {
       #Pomijamy te same kolumny
      if (kolumna1 != kolumna2) {
        # Tworzymy wykres korelacji
        wykres_korelacji <- ggplot(dane_grupa, aes(x = .data[[kolumna1]], y = .data[[kolumna2]])) +
          geom_point(aes(color = grupa)) +
          geom_smooth(method = "lm", se = FALSE, color = "blue") +
          labs(
            title = paste("Korelacja między", kolumna1, "a", kolumna2),
            x = kolumna1,
            y = kolumna2
          ) +
          theme_minimal()
        
        # Zapisz wykres do pliku
        
        
        
        ggsave(filename = paste("wykres_korelacji_", kolumna1, "_", kolumna2, "_", grupa, ".pdf", sep = ""), 
               plot = wykres_korelacji, width = 6, height = 4, dpi = 300, device = "pdf")
        
      
      }
    }
  }
}






