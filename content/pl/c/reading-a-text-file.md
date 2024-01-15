---
title:                "Odczytywanie pliku tekstowego"
html_title:           "C: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

C jest jednym z najpopularniejszych języków programowania na świecie i znajduje szerokie zastosowanie w różnych dziedzinach. W tym artykule dowiesz się, jak za pomocą C odczytać plik tekstowy i przetworzyć jego zawartość.

## Jak to zrobić

Przykład kodu w języku C:
```
#include <stdio.h>

int main() {
   FILE *plik;
   char linia[200];

   // otwieranie pliku
   plik = fopen("tekstowy.txt", "r");

   // sprawdzanie czy plik został otwarty poprawnie
   if (plik == NULL) {
      printf("Nie udało się otworzyć pliku.");
      return 1;
   }

   // czytanie linia po linii
   while (fgets(linia, 200, plik) != NULL) {
      // wyświetlanie zawartości linii na ekranie
      printf("%s", linia);
   }

   // zamykanie pliku
   fclose(plik);

   return 0;
}
```

Przykładowy plik tekstopwy "tekstowy.txt":
```
To jest przykładowy plik tekstowy.
Zawiera on kilka linii tekstu.
Możemy go odczytać za pomocą języka C.
```

Przykładowy wynik działania programu:
```
To jest przykładowy plik tekstowy.
Zawiera on kilka linii tekstu.
Możemy go odczytać za pomocą języka C.
```

## Głębsze zagadnienia

Odczytywanie pliku tekstowego w języku C wymaga wcześniejszego zapoznania się z pojęciem wskaźnika oraz funkcjami bibliotecznymi fopen i fgets. Należy również pamiętać o obsłudze błędów, takich jak nieudane otwarcie pliku. W przypadku dłuższych plików tekstowych, możemy użyć pętli while do odczytywania linii po linii. Możemy również wykorzystać funkcje takie jak fread lub fgetc do odczytywania pojedynczych znaków z pliku.

## Zobacz też

- [Dokumentacja języka C](https://pl.wikibooks.org/wiki/C/Dokumentacja)
- [Język C - Wikipedia](https://pl.wikipedia.org/wiki/J%C4%99zyk_C)
- [Kurs języka C - Programiz](https://www.programiz.com/c-programming)