---
title:                "C: Tworzenie pliku tekstowego"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest częstym zadaniem w programowaniu C. Pozwala nam ono na przechowywanie i przetwarzanie danych w czytelnej formie. Jest to również niezbędne do komunikacji z użytkownikiem lub innymi programami. W tym artykule dowiesz się jak tworzyć i manipulować tekstowymi plikami w języku C.

## Jak to zrobić

Aby pisać pliki tekstowe w języku C, potrzebujemy wykorzystać dwie funkcje z biblioteki standardowej - `fopen()` i `fprintf()`. Oto prosty przykład kodu, który zapisze tekst do pliku:

```C
#include <stdio.h>

int main()
{
    // otwórz plik w trybie do zapisu
    FILE *plik = fopen("tekst.txt", "w");

    // sprawdź czy plik został poprawnie otwarty
    if (plik == NULL) {
        printf("Nie udało się otworzyć pliku!");
        return 1;
    }

    // zapisz tekst do pliku
    fprintf(plik, "To jest przykładowy tekst.");

    // zamknij plik
    fclose(plik);

    return 0;
}
```

Powyższy kod otwiera plik o nazwie "tekst.txt" w trybie do zapisu i zapisuje w nim tekst "To jest przykładowy tekst.". Należy pamiętać, że jeśli plik o podanej nazwie już istnieje, zostanie on nadpisany. Aby uniknąć takiej sytuacji, możemy użyć trybu do dopisywania (`"a"`) lub stworzyć funkcję, która będzie sprawdzać istnienie pliku przed jego otwarciem.

Aby wczytywać dane z pliku tekstowego, używamy funkcji `fscanf()` w pętli while, dopóki nie osiągniemy końca pliku (`EOF`):

```C
while (fscanf(plik, "%s", slowo) != EOF) {
    printf("%s ", slowo);
}
```

## Deep Dive

Chcąc lepiej zrozumieć jak działa pisanie plików tekstowych, warto poznać wyrażenia formatujące używane przez funkcję `fprintf()`. Oto kilka najczęściej wykorzystywanych:

- `%s` - zapisuje ciąg znaków
- `%d` - zapisuje liczbę całkowitą
- `%f` - zapisuje liczbę zmiennoprzecinkową
- `%c` - zapisuje pojedynczy znak
- `%p` - zapisuje wskaźnik

Ponadto, warto również pamiętać o zamknięciu pliku po zakończeniu zapisu lub odczytu danych. Niezamknięte pliki mogą powodować błędy w programie lub utrudnić dalsze działania związane z danym plikiem.

## Zobacz również

- [Dokumentacja C - Pliki](https://www.w3schools.in/c-programming/file-handling/)
- [Tutorial C - Pliki tekstowe](https://www.programiz.com/c-programming/c-file-input-output)
- [Wideo tutorial - Pliki tekstowe w C](https://www.youtube.com/watch?v=5DyYAF9ynME)