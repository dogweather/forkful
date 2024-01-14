---
title:    "C: Odczytywanie pliku tekstowego"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak programy czytają pliki tekstowe? Czy zastanawiałeś się, jak można to zrobić w kodzie C? W tym artykule wyjaśnimy, dlaczego warto poznać mechanizm czytania plików tekstowych w C i jak to zrobić w praktyce.

## Jak to zrobić

Aby czytać pliki tekstowe w C, musimy wykonać kilka prostych kroków:

1. Otwórz plik przy użyciu funkcji `fopen()`, która zwraca wskaźnik do pliku.
2. Wykorzystaj pętlę `while` do czytania pliku linia po linii przy użyciu funkcji `fgets()`.
3. Zamknij plik za pomocą funkcji `fclose()`.

Przykładowy kod może wyglądać następująco:

```C
#include <stdio.h>

int main() {
  FILE *file; // wskaźnik do pliku
  char line[100]; // bufor na przechowywanie aktualnie czytanej linii

  // otwieramy plik w trybie tylko do odczytu
  file = fopen("plik.txt", "r");

  if (file == NULL) {
    printf("Nie można otworzyć pliku!");
    return 1;
  }

  // czytamy plik linia po linii i wypisujemy na ekran
  while (fgets(line, 100, file) != NULL) {
    printf("%s", line);
  }

  fclose(file); // zamykamy plik

  return 0;
}
```

## Dogłębna analiza

Jak widać, czytanie plików tekstowych w C jest bardzo proste. Jednak warto zauważyć kilka ważnych rzeczy:

- Funkcja `fopen()` przyjmuje dwa argumenty: nazwę pliku oraz tryb otwarcia. W naszym przykładzie użyliśmy trybu `r`, co oznacza tylko odczyt. Możemy również otworzyć plik w trybie `w` (tylko zapis), `a` (dodawanie na końcu pliku) lub `r+` (odczyt i zapis).
- Funkcja `fgets()` przyjmuje trzy argumenty: bufor na przechowywanie linii, maksymalną długość pojedynczej linii oraz wskaźnik do pliku.
- W pętli `while` sprawdzamy, czy funkcja `fgets()` zwraca wartość `NULL`, co oznacza koniec pliku.
- Aby uniknąć problemów z pamięcią, warto ustawić maksymalną długość linii w funkcji `fgets()` na mniejszą niż faktyczna maksymalna długość linii w naszym pliku.

## Zobacz również

Jeśli jesteś zainteresowany dowiedzeniem się więcej o funkcjach do czytania i pisania plików w języku C, polecamy zapoznanie się z poniższymi artykułami:

- [C File Handling Basics](https://www.programiz.com/c-programming/c-file-input-output)
- [The Standard Files in C](https://www.gnu.org/software/libc/manual/html_node/Standard-Files.html)

Dziękujemy za przeczytanie tego artykułu i mamy nadzieję, że teraz wiesz, jak czytać pliki tekstowe w C. W razie pytań lub uwag, możesz skorzystać z sekcji komentarzy poniżej. Do zobaczenia w kolejnym artykule!