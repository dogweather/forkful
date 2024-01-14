---
title:    "C: Tworzenie tymczasowego pliku"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest nieodłączną częścią programowania w języku C. Jest to bardzo przydatna technika, szczególnie przy pracy z dużymi danymi lub przy chęci zachowania danych w czasie działania programu. Tworzenie tymczasowych plików daje programiście większą kontrolę nad danymi i ułatwia debugowanie kodu.

## Jak to zrobić

Aby stworzyć tymczasowy plik w języku C, należy użyć funkcji "tmpfile()", która zwraca wskaźnik na plik tymczasowy. Następnie można wykorzystać ten wskaźnik do zapisywania danych do pliku.

Przykładowy kod wykorzystujący funkcję "tmpfile()":

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Tworzenie pliku tymczasowego
    FILE* tmp_file = tmpfile();

    // Sprawdzenie czy plik został utworzony
    if (tmp_file == NULL) {
        printf("Nie można utworzyć pliku tymczasowego.\n");
        exit(1);
    }

    // Zapisywanie danych do pliku
    fprintf(tmp_file, "To jest przykładowy tekst.");

    // Zamykanie pliku tymczasowego
    fclose(tmp_file);

    return 0;
}
```

Po uruchomieniu powyższego kodu, w bieżącym katalogu zostanie utworzony tymczasowy plik, w którym znajdzie się napis "To jest przykładowy tekst.".

## Głębszy zanurzanie

Tworzenie tymczasowych plików może być wykorzystywane w wielu różnych przypadkach, np. w celu zapisywania wyników działania programu lub przechowywania danych czasowo.

Warto pamiętać, że pliki tymczasowe są usuwane automatycznie po zamknięciu. Jeśli chcemy zachować dane z pliku tymczasowego, należy skopiować je do innego pliku przed jego zamknięciem. Dodatkowo, można określić własną ścieżkę do pliku tymczasowego za pomocą funkcji "tmpnam()".

## Zobacz także

- Dokumentacja funkcji "tmpfile()": https://www.programiz.com/c-programming/library-function/stdio.h/tmpfile
- Przykładowe zastosowania tworzenia plików tymczasowych: https://www.geeksforgeeks.org/tmpnam-function-in-c/
- Przykładowe projekty wykorzystujące pliki tymczasowe: https://github.com/topics/temporary-file