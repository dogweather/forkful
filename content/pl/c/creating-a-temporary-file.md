---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest nieodłączną częścią wielu programów w języku C. To ważna umiejętność, ponieważ pozwala na przechowywanie i manipulowanie danymi w sposób bezpieczny i skuteczny.

## Jak To Zrobić

Wykorzystując funkcję `tmpfile()` biblioteki standardowej języka C, można stworzyć tymczasowy plik w prosty sposób. Oto przykładowy kod:

```C
#include <stdio.h>

int main(void)
{
  FILE *tmp = tmpfile();

  if (tmp == NULL)
  {
    printf("Nie można utworzyć tymczasowego pliku.\n");
    return 1;
  }

  fputs("To jest przykładowy tekst.", tmp);

  // Otwórz i odczytaj zawartość tymczasowego pliku.
  fseek(tmp, 0, SEEK_SET);
  char c = fgetc(tmp);

  printf("%c\n", c); // Powinno wypisać "T".

  // Usuń tymczasowy plik.
  fclose(tmp);

  return 0;
}
```

Ten przykład wykorzystuje funkcję `fseek()` do ustawienia wskaźnika na początek pliku i następnie odczytuje pierwszy znak za pomocą funkcji `fgetc()`. Należy pamiętać, że wszystkie operacje na tymczasowym pliku należy wykonać przed jego zamknięciem, ponieważ po tym użytkownik nie ma już do niego dostępu.

## Deep Dive

Funkcja `tmpfile()` jest często używana podczas przetwarzania dużych plików lub w sytuacjach, gdy potrzebujemy tymczasowego miejsca do przechowywania danych. Wtedy też może się okazać, że stworzenie i usunięcie tymczasowego pliku jest szybsze niż użycie pamięci zwalnianej przez funkcję `malloc()`.

Istnieje również możliwość tworzenia nazwanych tymczasowych plików, dzięki wykorzystaniu funkcji `mkstemp()`. Pozwala to na lepszą kontrolę nad plikiem, ponieważ mamy możliwość nadania mu własnej nazwy.

## Zobacz też

- [Poradnik do języka C](https://pl.wikibooks.org/wiki/Programowanie/Jezyk_C)
- [Dokumentacja funkcji `tmpfile()`](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Poradnik do pracy z plikami w języku C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)