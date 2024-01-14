---
title:    "C: Tworzenie pliku tymczasowego"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest częstym zagadnieniem w programowaniu w języku C. Często jest to potrzebne, gdy chcemy przechować tymczasowe dane, które nie muszą być trwałe. Są one również przydatne, gdy potrzebujemy dostępu do tymczasowych plików w naszym programie. W tym blogu dowiesz się jak stworzyć tymczasowe pliki w C i jak z nich korzystać.

## Jak to zrobić

Stworzenie tymczasowego pliku w języku C jest bardzo proste. Musimy skorzystać z funkcji `tmpfile()`, która tworzy nowy plik tymczasowy w systemie i zwraca wskaźnik na ten plik. Oto przykładowy kod:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  FILE *temp_file = tmpfile(); // stworzenie tymczasowego pliku
  if (temp_file == NULL) { // sprawdzenie czy plik został stworzony
    printf("Nie można stworzyć tymczasowego pliku!");
    exit(1);
  }
  fprintf(temp_file, "To jest tekst tymczasowy."); // zapisanie tekstu do pliku
  fclose(temp_file); // zamknięcie pliku
  return 0;
}
```

W powyższym przykładzie tworzymy tymczasowy plik za pomocą funkcji `tmpfile()` i zapisujemy do niego przykładowy tekst za pomocą funkcji `fprintf()`. Następnie zamykamy plik przy użyciu funkcji `fclose()`.

Po wykonaniu powyższego kodu, w naszym systemie zostanie stworzony nowy plik tymczasowy. Możemy również odczytać zawartość tego pliku za pomocą funkcji `fopen()` i `fread()`. Oto przykładowy kod:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  FILE *temp_file = tmpfile(); // stworzenie tymczasowego pliku
  if (temp_file == NULL) { // sprawdzenie czy plik został stworzony
    printf("Nie można stworzyć tymczasowego pliku!");
    exit(1);
  }
  fprintf(temp_file, "To jest tekst tymczasowy."); // zapisanie tekstu do pliku
  fseek(temp_file, 0, SEEK_SET); // ustawienie wskaźnika na początek pliku
  char text[100]; // tablica do przechowywania odczytanego tekstu
  fread(text, 1, 100, temp_file); // odczytanie tekstu z pliku
  printf("%s", text); // wyświetlenie tekstu na ekranie
  fclose(temp_file); // zamknięcie pliku
  return 0;
}
```

W powyższym przykładzie, po ustawieniu wskaźnika pliku na początek, odczytujemy jego zawartość przy użyciu funkcji `fread()` i wyświetlamy ją na ekranie za pomocą funkcji `printf()`.

## Deep Dive

Tworzenie tymczasowych plików w języku C może być także nieco bardziej skomplikowane, jeśli chcemy dostać dostęp do pliku poza naszym programem. W takim przypadku musimy wykorzystać nazwy tymczasowe plików. Oto przykładowy kod:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  char temp_filename[L_tmpnam]; // tablica do przechowywania nazwy pliku
  tmpnam(temp_filename); // wygenerowanie nazwy tymczasowego pliku
  FILE *temp_file = fopen(temp_filename, "w"); // otwarcie pliku do zapisu
  if (temp_file == NULL) { // sprawdzenie czy plik został otwarty
    printf("Nie można otworzyć pliku!");
    exit(1);
  }
  fprintf(temp_file, "To jest tekst tymczasowy."); // zapisanie tekstu do pliku
  fclose(temp_file); // zamknięcie pliku
  printf("Nazwa tymczas