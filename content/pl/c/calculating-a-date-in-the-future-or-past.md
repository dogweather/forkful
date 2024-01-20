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

## Co & Dlaczego?

Obliczanie przyszłego lub przeszłego terminu to określanie daty, która będzie następowała po określonym czasie lub która wystąpiła przed danym okresem. Programiści to robią, aby przetrzymywać, sortować i manipulować danymi związanymi z datą w aplikacjach.

## Jak to zrobić:

Podajemy sposób obliczania daty w przyszłości lub przeszłości za pomocą funkcji „mktime” i „localtime” w języku C. Załóżmy, że chcemy dodać 15 dni do bieżącej daty:

```C
#include <stdio.h> 
#include <time.h> 

int main() { 
    // Uzyskujemy obecny czas
    time_t obecny_czas = time(NULL); 
    struct tm *czas_tm = localtime(&obecny_czas);
    // Dodajemy 15 dni
    czas_tm->tm_mday += 15;
    // Konwertujemy strukturę z powrotem na czas
    time_t nowy_czas = mktime(czas_tm);
      
    printf("Data po 15 dniach: %s", ctime(&nowy_czas)); 
      
    return 0; 
} 
```

Wyjście:

```C
Data po 15 dniach: Sun Jan 30 12:40:08 2022
```

## Deep Dive:

Historia: W przeszłości, bez bibliotek do zarządzania datą i czasem, obliczanie dat przeszłych lub przyszłych wymagało ręcznego manipulowania datami, co było skomplikowane i podatne na błędy.

Alternatywy: Inne języki programowania, takie jak Python czy Java, mają wbudowane metody do obliczania dat przeszłych lub przyszłych bez konieczności pisania tak wiele kodu.

Szczegóły implementacji: Zastanów się nad przypadek, gdy dodajesz dni do daty i wynik przekracza ostatni dzień miesiąca. Dzień jest zerowany, a miesiąc jest zwiększany o jeden. Jeśli nasze obliczenia przeniosą nas do następnego roku, rok zostanie również zwiększony.

## Zobacz również:

1. [Dokumentacja języka C - funkcja mktime](https://en.cppreference.com/w/c/chrono/mktime)
2. [StackOverflow: How to add days to the current date?](https://stackoverflow.com/questions/1442116/how-to-get-the-date-of-7-days-ago-in-c)