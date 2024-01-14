---
title:    "C++: Tworzenie tymczasowego pliku"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest nieodzownym elementem programowania w C++. Pliki te służą jako pomocnicze do przechowywania danych, które zostaną usunięte po zakończeniu działania programu. Jest to przydatne w przypadku gdy nie chcemy zanieczyszczać przestrzeni dyskowej lub chcemy chronić nasze dane przed niepożądanym dostępem.

## Jak to zrobić?

Aby utworzyć tymczasowy plik w C++, potrzebujemy wykorzystać funkcje z biblioteki <cstdlib>. Najważniejsze są tu dwie funkcje: "tmpnam" i "tmpfile". Pierwsza zwraca unikalną nazwę tymczasowego pliku, której możemy użyć do jego utworzenia. Druga natomiast tworzy i otwiera plik tymczasowy w trybie binarnym.

```C++
#include <cstdlib>
#include <cstdio>

int main() {
  // Tworzenie tymczasowego pliku
  char* nazwa = tmpnam(nullptr);
  FILE* plik = fopen(nazwa, "w+b");

  // Zapis do pliku
  const char* tekst = "To jest przykładowy tekst";
  fwrite(tekst, sizeof(char), strlen(tekst), plik);

  // Odczyt z pliku
  char bufor[50];
  fseek(plik, 0, SEEK_SET);
  fread(bufor, sizeof(char), strlen(tekst), plik);

  // Wyświetlanie zawartości pliku
  printf("Zawartość pliku: %s\n", bufor);

  // Zamykanie pliku i usuwanie po nim
  fclose(plik);
  remove(nazwa);

  return 0;
}
```

W powyższym przykładzie tworzymy tymczasowy plik, zapisujemy do niego tekst, a następnie odczytujemy i wyświetlamy jego zawartość. Na koniec zamykamy plik i używamy funkcji "remove" aby go usunąć.

## Pogłębione informacje

Głębsze zrozumienie tworzenia tymczasowych plików może być przydatne w bardziej skomplikowanych projektach. Warto wiedzieć, że funkcja "tmpnam" używa zmiennej globalnej, dlatego nie jest wątkowa. Natomiast funkcja "tmpfile" tworzy plik w aktualnym katalogu roboczym, dlatego może być konieczne wykorzystanie funkcji "mkstemp" w celu wybrania własnego katalogu. Ponadto, warto zabezpieczyć się przed dopisywaniem do istniejącego już pliku tymczasowego, używając funkcji "mkostemp" z flagą O_EXCL.

## Zobacz też

- [Dokumentacja C++: Tworzenie Tymczasowych Plików](https://en.cppreference.com/w/cpp/io/c/tmpnam)
- [Dokumentacja C: Tworzenie Tymczasowych Plików](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)
- [Poradnik: Tworzenie Tymczasowych Plików w C++](https://www.tutorialspoint.com/how-to-create-a-temporary-file-in-cplusplus-programming)