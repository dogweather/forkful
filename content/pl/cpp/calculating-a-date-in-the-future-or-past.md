---
title:    "C++: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Obliczenie daty w przyszłości lub przeszłości jest częstym problemem w programowaniu. Jest to szczególnie przydatne przy tworzeniu aplikacji, które wymagają wyświetlania dat lub podawania przyszłych terminów. W tym artykule dowiesz się, jak w prosty sposób obliczyć datę w przyszłości lub przeszłości za pomocą języka C++.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości, musimy wykorzystać kilka funkcji w języku C++. W pierwszej kolejności musimy uwzględnić datę bazową, czyli dzisiejszą datę, od której będziemy obliczać przyszłą lub przeszłą datę. Następnie musimy dodać lub odjąć odpowiednią liczbę dni, miesięcy lub lat w zależności od tego, jak daleko w przyszłość lub przeszłość chcemy się przenieść.

Przykładowy kod C++ do obliczenia przyszłej daty za pomocą funkcji "tm":

```C++
#include <iostream>
#include <ctime>

int main()
{
    // dzisiejsza data
    time_t now = time(0);
    tm *ltm = localtime(&now);

    // dodanie 10 dni do dzisiejszej daty
    ltm->tm_mday += 10;
    mktime(ltm);

    // wyświetlenie daty po dodaniu 10 dni
    std::cout << "Przyszła data: " << ltm->tm_mday << "." << ltm->tm_mon + 1 << "." << ltm->tm_year + 1900 << std::endl;

    return 0;
}
```

W tym przykładzie najpierw używamy funkcji "time" do pobrania dzisiejszej daty, a następnie przypisujemy ją do obiektu "tm", który jest używany do przechowywania dat w języku C++. Następnie dodajemy 10 dni do daty i ponownie używamy funkcji "mktime" do przeliczenia daty. Wreszcie, wyświetlamy przyszłą datę na ekranie.

## Deep Dive

Poza funkcją "tm", język C++ oferuje również wiele innych funkcji, które mogą pomóc w obliczeniu przyszłych lub przeszłych dat. Na przykład, funkcja "localtime" pozwala przekonwertować czas na lokalną strefę czasową, a funkcja "mktime" dokonuje odwrotnej konwersji. Istnieją też gotowe biblioteki, takie jak "boost::date_time", które oferują zaawansowane funkcje do manipulowania datami.

Więcej informacji na temat obliczania dat w przyszłości lub przeszłości można znaleźć w dokumentacji języka C++ oraz różnych podręcznikach programistycznych.

## Zobacz również

* [Dokumentacja języka C++](https://en.cppreference.com/w/cpp/chrono)
* [Przykładowy kod do obliczania daty w przyszłości](https://www.geeksforgeeks.org/calculating-date-time-in-c/)
* [Biblioteka boost::date_time](https://www.boost.org/doc/libs/1_70_0/doc/html/date_time.html)