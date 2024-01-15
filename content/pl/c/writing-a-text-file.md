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

## Dlaczego

Pisanie pliku tekstowego jest jedną z podstawowych operacji w programowaniu w języku C. Jest to niezbędne, jeśli chcemy móc zapisywać trwałe dane, takie jak konfiguracje, wyniki działania programu czy informacje o użytkownikach.

## Jak to zrobić

Aby zapisać plik tekstowy w języku C, należy wykonać kilka kroków. Najpierw musimy otworzyć plik za pomocą funkcji `fopen()`, która przyjmuje dwa argumenty - nazwę pliku oraz tryb otwarcia. Następnie możemy użyć funkcji `fprintf()` do zapisania danych do pliku. Na koniec musimy pamiętać o zamknięciu pliku za pomocą funkcji `fclose()`.

````C
#include <stdio.h>

int main() {
    // otwieramy plik w trybie do zapisu
    FILE *fp = fopen("plik.txt", "w");

    // zapisujemy tekst do pliku
    fprintf(fp, "Witaj, to jest przykładowy tekst!");

    // zamykamy plik
    fclose(fp);

    return 0;
}
````

Po uruchomieniu tego programu, w bieżącym folderze powinien pojawić się plik o nazwie "plik.txt" zawierający nasz tekst.

## Gleboki zanurkowanie

Pisząc plik tekstowy w języku C, możemy również wykorzystać funkcję `fputc()` do zapisywania pojedynczego znaku oraz funkcję `fputs()` do zapisywania całych łańcuchów znaków. Ważne jest również pamiętanie o prawidłowym obsługiwaniu błędów, wykonując sprawdzenie czy funkcja `fopen()` zwróciła wartość różną od `NULL` - w przeciwnym razie oznacza to, że plik nie został otwarty prawidłowo.

## Zobacz również

- [Dokumentacja funkcji fopen()](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Kurs języka C na Codecademy](https://www.codecademy.com/learn/learn-c) 
- [Inne artykuły na temat programowania w języku C](https://pl.wikibooks.org/wiki/Programowanie_w_j%C4%99zyku_C)