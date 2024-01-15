---
title:                "Uzyskanie aktualnej daty"
html_title:           "C++: Uzyskanie aktualnej daty"
simple_title:         "Uzyskanie aktualnej daty"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdej osobie używającej komputera zdarza się kiedyś potrzebować aktualnej daty. Może to być do celów raportowania, ustawiania terminów, lub po prostu dla własnej świadomości. W tym artykule dowiesz się, jak w prosty sposób uzyskać aktualną datę w języku C++, co może być przydatne w wielu sytuacjach.

## Jak to zrobić


```C++
#include <iostream>
#include <ctime>

int main() {

    // deklaracja i inicjalizacja zmiennej czasowej
    time_t now = time(0);

    // konwersja czasu na lokalną reprezentację
    tm* local_time = localtime(&now);

    // wyświetlenie aktualnej daty
    std::cout << "Aktualna data: " 
              << local_time->tm_mday << "/" 
              << local_time->tm_mon + 1 << "/" 
              << local_time->tm_year + 1900 << std::endl;

    return 0;
}
```

```bash
$ g++ current_date.cpp -o current_date
$ ./current_date
Aktualna data: 6/4/2021
```

W powyższym przykładzie najpierw do zmiennej `now` przypisujemy aktualny czas wyrażony w sekundach od 1 stycznia 1970 roku. Następnie używając funkcji `localtime` konwertujemy ten czas na lokalną reprezentację, czyli dzień, miesiąc i rok. Na koniec, za pomocą obiektu `tm` wyświetlamy odpowiednie elementy daty.

## Deep Dive

C++ posiada wiele funkcji związanych z operacjami na czasie i dacie. W przykładzie wykorzystaliśmy funkcję `time` oraz `localtime`, ale istnieją też inne sposoby na uzyskanie aktualnej daty, takie jak na przykład funkcja `asctime`, która pozwala na wyświetlenie daty w czytelnej dla człowieka postaci. Warto zwrócić uwagę, że funkcje związane z czasem i datami różnią się w zależności od systemu operacyjnego, dlatego warto sprawdzić ich dokumentację przed wykorzystaniem.

## Zobacz także

- [cppreference - time](https://en.cppreference.com/w/cpp/chrono/time_point)
- [GeeksforGeeks - C++ Date and Time](https://www.geeksforgeeks.org/date-and-time-manipulation-in-c/)
- [cplusplus.com - Dates and Times in C++](https://www.cplusplus.com/reference/ctime/)