---
title:                "Obliczanie daty w przyszłości lub przeszłości."
html_title:           "C: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Kalkulacja daty w przeszłości lub przyszłości może być przydatna w różnych scenariuszach programowania. Na przykład, jeśli tworzysz aplikację kalendarza, musisz umieścić funkcję, która umożliwia użytkownikom wybór daty przyszłych lub przeszłych do wyświetlenia. W takich przypadkach, znajomość programowania w języku C może być niezbędna.

## Jak to zrobić

Aby móc obliczyć datę w przeszłości lub przyszłości w języku C, należy wykorzystać funkcję time.h inicjowaną z systemu czasu. Poniżej przedstawiamy przykładowy kod dla obliczenia daty sześć miesięcy do tyłu od bieżącej daty:

```C
#include <stdio.h>
#include <time.h>

int main() {
    int current_day, current_month, current_year;
    struct tm *current_time;
    time_t current_seconds;
    
    // uzyskaj bieżącą datę i godzinę
    time(&current_seconds);
    current_time = localtime(&current_seconds);
    
    // przyporządkuj wartości bieżącego dnia, miesiąca i roku
    current_day = current_time->tm_mday;
    current_month = current_time->tm_mon + 1;
    current_year = current_time->tm_year + 1900;
    
    // oblicz datę sześć miesięcy do tyłu
    current_month = current_month - 6;
    
    // jeśli bieżący miesiąc jest mniejszy niż 1, zmniejsz o 12 i zwiększ rok o 1
    if (current_month < 1) {
        current_month = current_month + 12;
        current_year = current_year + 1;
    }
    
    printf("Data sześć miesięcy do tyłu od dzisiaj to:%02d/%02d/%d", current_day, current_month, current_year);
    
    return 0;
}
```
**Wynik: Data sześć miesięcy do tyłu od dzisiaj to: 23/01/2021**

## Głębsza analiza

Obliczanie daty w przeszłości lub przyszłości może być dokonane poprzez modyfikację bieżących wartości dnia, miesiąca i roku. W powyższym przykładzie, przy użyciu funkcji time.h, bieżąca data i godzina są odczytywane i umieszczane w strukturze tm, a następnie przypisane do zmiennych. Następnie, może być użyta odpowiednia funkcja zależna od celu obliczeń, na przykład w przypadku obliczenia daty przyszłej, należy dodać odpowiednią liczbę dni do bieżącego dnia, miesiąca i roku.

## Zobacz także

- [Dokumentacja funkcji time.h w języku C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Przykładowy kod obliczający datę w przeszłości lub przyszłości w języku C++](https://www.programiz.com/cpp-programming/library-function/ctime/mktime)