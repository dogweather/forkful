---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "C++: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości polega na dodawaniu lub odejmowaniu pewnej liczby dni do określonej daty. Programiści robią to, aby śledzić terminy, generować harmonogramy, zarządzać danymi historycznymi i wiele więcej.

## Jak to zrobić:

Rozważmy prosty przykład, w którym dodajemy siedem dni do dzisiejszej daty za pomocą standardowej biblioteki C++. 

```C++
#include <iostream>
#include <chrono>

int main() {
    std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
    std::chrono::system_clock::time_point week_later = today + std::chrono::hours(24*7);

    std::time_t tt = std::chrono::system_clock::to_time_t( week_later );
    std::tm * ptm = std::localtime(&tt);
    std::cout<<"Date one week later: "<<std::put_time(ptm,"%c")<<std::endl;
}
```
Wyjście wynikowe będzie datą jednego tygodnia od dzisiaj.

## Głębsze spojrzenie:

Historia obliczeń dat przypomina historię prognozowania pogody - rozpoczęła się dużo wcześniej, niż możemy myśleć, i stała się coraz bardziej zaawansowana z technologicznym czasem. Przed maszynami, ludzie liczyli daty przyszłe i przeszłe za pomocą kalendarzy, księżycowych cykli i obserwacji sezonowych. 

Z alternatyw obliczania dat w przyszłości lub przeszłości w C++, poza std::chrono, istnieje wiele innych bibliotek trzecich, takich jak date, Boost.Date_Time, ICU (International Components for Unicode) i wiele innych. Każda z nich ma swoje zalety i wady, dlatego ważne jest, aby wybrać tę najbardziej odpowiednią do Twojego konkretnego zastosowania. 

W przypadku korzystania z biblioteki `std::chrono`, wszystko opiera się na punktach czasowych (`time_point`), które są określone jako liczba "kroków" od określonego punktu czasu zwanej "erą". 

## Zobacz również:

1. [Chrono w C++](https://en.cppreference.com/w/cpp/chrono)
2. [Biblioteka date dla C++](https://github.com/HowardHinnant/date)
3. [Biblioteka Boost.DateTime](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)
4. [ICU - International Components for Unicode](https://unicode-org.github.io/icu/userguide/datetime/calendar/)