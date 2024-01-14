---
title:                "C++: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Czas jest nieodłączną częścią naszego życia. W programowaniu również często wykorzystujemy informacje o aktualnej dacie do różnych celów, takich jak wyświetlenie jej na ekranie czy przeprowadzenie obliczeń. W tym artykule dowiesz się, jak w prosty sposób uzyskać aktualną datę w języku C++, dzięki czemu będziesz mógł wykorzystywać ją w swoich projektach.

## Jak to zrobić

W języku C++ do uzyskania aktualnej daty wykorzystujemy bibliotekę <ctime>. Najprostszym sposobem jest użycie funkcji time(), która zwróci nam liczbę sekund, które upłynęły od północy 1 stycznia 1970 roku (tzw. epoch). Przykładowy kod wykorzystujący tę funkcję może wyglądać tak:

```C++
#include <iostream>
#include <ctime>

int main() {
    time_t currentTime;
    time(&currentTime);

    std::cout << "Aktualna data i godzina: " << ctime(&currentTime) << std::endl;

    return 0;
}
```

Po uruchomieniu programu otrzymamy na ekranie coś w stylu "Aktualna data i godzina: Sat Oct 30 15:26:12 2021", w zależności od aktualnego czasu.

Możemy również sformatować tę datę za pomocą funkcji strftime(), która pozwala nam na wybór odpowiedniego formatu. Na przykład, jeśli chcemy wyświetlić datę w formacie "dd-mm-YYYY", możemy to zrobić w ten sposób:

```C++
#include <iostream>
#include <ctime>
#include <iomanip>

int main() {
    time_t currentTime;
    time(&currentTime);

    char buffer[80];
    struct tm * now = localtime(&currentTime);
    strftime(buffer, 80, "%d-%m-%Y", now);

    std::cout << "Aktualna data: " << buffer << std::endl;

    return 0;
}
```

Po uruchomieniu tego kodu otrzymamy na ekranie "Aktualna data: 30-10-2021".

## Głębszy zanurzenie

Ważne jest, aby pamiętać, że funkcje time() i strftime() zwracają lokalny czas dla naszej strefy czasowej. Jeśli chcemy uzyskać czas uniwersalny (UTC), musimy użyć funkcji gmtime() zamiast localtime(). Możemy również wyświetlić datę w formie liczbowej, korzystając z funkcji difftime(), która oblicza różnicę czasu między dwoma datami.

Inną ciekawą funkcją związaną z datami jest mktime(), która przekształca informacje o dacie i godzinie w liczbę sekund od epoch. Możemy również zmieniać datę w dowolnym momencie, korzystając z funkcji timegm() i zmiennych strukturalnych tm. Warto także zapoznać się z biblioteką <chrono>, która jest częścią standardowej biblioteki C++ i oferuje wiele narzędzi do obsługi czasu.

## Zobacz także

* [Poradnik o bibliotece <ctime> w języku C++](https://www.cplusplus.com/reference/ctime/)
* [Dokumentacja funkcji time() w C++](https://www.cplusplus.com/reference/ctime/time/)
* [Przykładowe użycie funkcji strftime() w C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)

Dziękujemy za przeczytanie tego artykułu. Mamy nadzieję, że zdobyłeś nowe informacje na temat uzyskiwania aktualnej daty w języku C++, które będą przydatne w Twoich projektach.