---
title:                "C: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest coraz bardziej popularne i rozwijające się dziedziną. Jednym z najważniejszych zadań programisty jest umiejętne manipulowanie tekstem, niezależnie od tego, czy jest to prosta gra czy też ogromna baza danych. Właśnie dlatego pisanie plików tekstowych jest niezwykle ważne w programowaniu.

## Jak to zrobić

Aby napisać plik tekstowy w języku C, musimy skorzystać z funkcji "fprintf". Najpierw należy otworzyć plik za pomocą funkcji "fopen", podając nazwę pliku oraz tryb, w jakim chcemy go otworzyć (np. do odczytu lub zapisu). Następnie, używamy funkcji "fprintf" do zapisywania tekstu w pliku, podając jako argumenty uchwyt pliku oraz tekst, który chcemy zapisać. Poniżej znajduje się przykładowy kod:

```C
#include <stdio.h>

int main () {
   // otwarcie pliku do zapisu
   FILE *plik = fopen("plik.txt", "w");
    
   // zapisanie tekstu do pliku
   fprintf(plik, "To jest przykładowy tekst.");
   
   // zamknięcie pliku
   fclose(plik);
      
   return 0;
}
```

Po uruchomieniu tego kodu, w folderze projektu pojawi się plik "plik.txt" zawierający nasz tekst.

## Głębszy zanurzenie

Istnieje wiele zaawansowanych metod pisania plików tekstowych w języku C, takich jak dynamiczne alokacje pamięci, przetwarzanie wielu linii tekstu oraz wykorzystanie struktur danych. Wymaga to jednak zaawansowanej wiedzy programistycznej i doświadczenia. Warto również pamiętać o poprawnym obsługiwaniu błędów podczas pisania i odczytywania plików.

## Zobacz również

- [Dokumentacja funkcji fprintf w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Przykładowe kody pisania plików tekstowych w języku C](https://www.geeksforgeeks.org/file-handling-c-classes/)
- [Artykuł o podstawach pisania i odczytywania plików w języku C](https://www.codingunit.com/c-tutorial-file-io-using-text-files)