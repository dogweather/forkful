---
title:                "C: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

Konwersja daty na napis jest powszechną potrzebą w wielu programach. Często musimy wyświetlić datę w czytelnej formie dla użytkowników aplikacji lub zapisywać ją do pliku w odpowiednim formacie. W tej krótkiej instrukcji dowiesz się, jak w prosty sposób przekonwertować datę na napis w języku C.

# Jak to zrobić

```c
#include <stdio.h>
#include <time.h>

int main()
{
    // deklarujemy zmienną typu time_t zawierającą aktualną datę i czas
    time_t now = time(NULL);
    // tworzymy napis przechowujący przekonwertowaną datę i czas
    char date[20];
    // wykorzystujemy funkcję strftime do przekonwertowania daty na napis w podanym formacie
    strftime(date, 20, "%d-%m-%Y %H:%M:%S", localtime(&now));
    // wyświetlamy przekonwertowaną datę
    printf("Aktualna data i godzina: %s", date);

    return 0;
}
```
Teraz, po uruchomieniu programu, zobaczymy w konsoli napis w formacie "DD-MM-RRRR GG:MM:SS", na przykład "01-03-2021 12:01:30".

# Głębsza analiza

W języku C istnieje kilka funkcji, które umożliwiają przekonwertowanie daty na napis. Jedną z najpopularniejszych jest funkcja `strftime`, która pozwala formatować datę według własnych potrzeb. Pierwszym argumentem funkcji jest tablica przechowująca napis, drugim argumentem jest maksymalna długość napisu, a trzecim argumentem jest format daty. W przykładzie wykorzystaliśmy format "%d-%m-%Y %H:%M:%S", który oznacza dzień-miesiąc-rok godzina:minuta:sekunda.

# Zobacz też

* Dokumentacja funkcji `strftime`: https://www.cplusplus.com/reference/ctime/strftime/
* Przykładowe formaty daty w funkcji `strftime`: https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm
* Inne sposoby na konwersję daty na napis w języku C: https://en.wikibooks.org/wiki/C_Programming/DateTime_functions