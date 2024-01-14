---
title:    "C++: Wyodrębnianie podciągów"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Dlaczego warto uczyć się wydzielania podciągów w języku C++?

W dzisiejszym świecie programowania, coraz większym wyzwaniem staje się efektywne korzystanie z ogromnych ilości danych. W wielu przypadkach nie potrzebujemy całego tekstu, a jedynie jego fragmentów. Tymczasem wydzielanie podciągów w języku C++ może okazać się niezwykle przydatne w codziennej pracy, a nauka tej umiejętności może przynieść wiele korzyści.

## Jak to zrobić?

Aby wydobyć podciągi z tekstu w języku C++, można posłużyć się funkcją `substr`. Przyjmujemy, że mamy zmienną przechowującą tekst oraz dwie zmienne przechowujące początkowy i końcowy indeks podciągu, który chcemy wydobyć. W takim przypadku, kod może wyglądać następująco:

```C++
// deklaracja zmiennej przechowującej tekst
string tekst = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";

// deklaracja zmiennych przechowujących indeks początkowy i końcowy podciągu
int poczatkowy_indeks = 6; // indeks zaczynający się od 0
int koncowy_indeks = 11;

// wydobycie podciągu z tekstu
string podciag = tekst.substr(poczatkowy_indeks, koncowy_indeks - poczatkowy_indeks + 1);

// wypisanie wydobytego podciągu
cout << podciag << endl;

// output: ipsum
```

## Deep Dive

Funkcja `substr` działa na obiekcie typu `string` i jako argumenty przyjmuje indeks początkowy oraz liczbę znaków do wydobycia. W przypadku podania błędnego indeksu lub liczby większej niż długość tekstu, funkcja zwróci pusty string. Oprócz funkcji `substr`, w języku C++ istnieje również możliwość wydobycia podciągu za pomocą metody `find` oraz operatora `[]`. Warto poznać wszystkie te sposoby, aby móc wybrać najbardziej efektywny w danym przypadku.

# Zobacz również

- Dokumentacja funkcji `substr` w języku C++: https://www.cplusplus.com/reference/string/string/substr/
- Inne sposoby na wydzielanie podciągów w języku C++: https://thispointer.com/how-to-extract-substring-from-a-string-in-c/
- Przykładowe zastosowania wydzielania podciągów: https://www.geeksforgeeks.org/interesting-facts-about-string-find-and-substr-in-c/