---
title:                "Tworzenie pliku tekstowego"
html_title:           "C: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

C - Pisanie pliku tekstowego

## Co to jest i dlaczego to robimy?

Pisanie pliku tekstowego w języku C polega na zapisaniu danych w formie czytelnej dla człowieka, zwykle w formacie tekstu. Jest to przydatne w przypadku przechowywania informacji lub tworzenia konfiguracji programów. Programiści często korzystają z tej metody, ponieważ pliki tekstowe są łatwe w modyfikacji i dostosowaniu pod indywidualne potrzeby.

## Jak to zrobić:

```C
#include <stdio.h>
 
int main() {
   FILE *plik;

   // otwórz plik tekstowy "moj_plik.txt" do zapisu 
   plik = fopen("moj_plik.txt", "w");

   // sprawdź, czy plik został pomyślnie otwarty
   if (plik == NULL) {
      printf("Nie można otworzyć pliku!");
      return -1;
   }

   // zapisz tekst w pliku
   fprintf(plik, "To jest tekst do zapisania w pliku\n");

   // zamknij plik
   fclose(plik);

   // wyświetl informację o pomyślnym zapisaniu pliku
   printf("Plik został pomyślnie zapisany!");

   return 0;
}
```

**Wyjście:**

W pliku "moj_plik.txt" pojawi się następujący tekst:

```text
To jest tekst do zapisania w pliku
```

## Głębsze zagadnienia:

**Kontekst historyczny:** Pisanie plików tekstowych jest jedną z najstarszych metod zapisu danych, sięgającą początków informatyki. Współcześnie, z powodu rozwoju technologii, zaczęto stosować również inne formaty, takie jak bazy danych czy pliki binarne.

**Alternatywy:** Istnieją również inne sposoby na przechowywanie danych w języku C, takie jak zapisywanie do plików binarnych lub używanie baz danych. Każda z tych metod ma swoje zalety i w zależności od konkretnego zadania, programiści wybierają najbardziej odpowiednią.

**Szczegóły implementacji:** Podczas pisania plików tekstowych, konieczne jest odpowiednie formatowanie tekstu, tak aby był czytelny dla użytkownika. Oprócz tego, ważne jest również prawidłowe otwieranie i zamykanie pliku, oraz przeprowadzenie odpowiednich operacji na danych przed zapisaniem ich w pliku.

## Zobacz także:

Dla więcej informacji na temat pisania plików tekstowych w języku C, zapoznaj się z dokumentacją [stdio.h](https://www.tutorialspoint.com/c_standard_library/stdio_h.htm).