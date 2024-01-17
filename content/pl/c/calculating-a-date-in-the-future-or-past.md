---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "C: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości jest po prostu wyliczeniem daty wynikowej w zależności od podanej daty początkowej i liczby dni lub lat do dodania lub odjęcia. Programiści często wykorzystują to do tworzenia aplikacji, takich jak kalendarze, planery lub systemy rezerwacji.

## Jak to zrobić:

Kodowanie takiej funkcji w języku C jest bardzo proste. Poniżej przedstawiono przykłady dla obliczania daty w przyszłości i przeszłości.
```
C
#include <stdio.h>
#include <time.h>

void addDays(int days){
    time_t now;
    struct tm *date;
    
    time(&now);
    date = localtime(&now);
    
    date->tm_mday = date->tm_mday + days;

    mktime(date);
    printf("Data wynikowa: %d.%d.%d", date->tm_mday, date->tm_mon+1, date->tm_year+1900);
}

void main(){
    //obliczanie daty 30 dni w przyszłości
    addDays(30);
}
/*
Data wynikowa: 31.12.2019
*/
```

```
C
#include <stdio.h>
#include <time.h>

void subtractYears(int years){
    time_t now;
    struct tm *date;
    
    time(&now);
    date = localtime(&now);
    
    date->tm_year = date->tm_year - years;
    
    mktime(date);
    printf("Data wynikowa: %d", date->tm_year+1900);
}

void main(){
    //obliczanie roku sprzed 30 lat
    subtractYears(30);
}
/*
Data wynikowa: 1989
*/
```

## Zanurzenie:

Obliczanie dat w przeszłości i przyszłości jest możliwe dzięki wykorzystaniu funkcji z biblioteki <time.h> w języku C. Istnieją również inne sposoby na to, takie jak wykorzystanie bibliotek zewnętrznych lub wykorzystanie gotowych funkcji z innych języków programowania, na przykład z Pythona.

## Zobacz także:

- [Dokumentacja biblioteki time.h w języku C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Przykłady kodów obliczających daty w różnych językach programowania](https://www.geeksforgeeks.org/program-date-time-programs/)