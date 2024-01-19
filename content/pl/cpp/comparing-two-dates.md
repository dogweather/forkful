---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Porównywanie dwóch dat polega na ustaleniu, która data jest wcześniejsza, późniejsza, lub czy obie są takie same. Programiści robią to, żeby monitorować i sortować wydarzenia chronologicznie w ich aplikacjach.

## Jak to zrobić:
Poniżej znajdują się przykłady kodowania i próbki wyjściowe:

```C++
#include <iostream>
#include <chrono> 
using namespace std;
int main () {
  chrono::system_clock::time_point today = chrono::system_clock::now();
  chrono::system_clock::time_point past = chrono::system_clock::now() - chrono::hours(24*365);

  if(today > past)
    cout << "Today is later than one year ago.";
  else if(today == past)
    cout << "Impossible! Time travel?";
  else
    cout << "Something went wrong!";
  
  return 0;
}
```
Gdy uruchomisz ten kod, wyświetli ci się napis "Today is later than one year ago."

## Pogłębione zagłębienie:
Porównywanie dat bywało trudniejsze, zanim wprowadzono bibliotekę chrono w C++11. Wcześniej trzeba było używać czasu strukturalnego i robić dużo ręcznego porównywania. Alternatywnie, można użyć innych bibliotek, takich jak Boost.DateTime.

Choć biblioteka chrono sprawia, że porównywanie dat jest prostsze, nadal wymaga zrozumienia podstawowych jednostek czasu. Biblioteka traktuje daty jako punkty w czasie i pozwala na porównywanie tych punktów bezpośrednio.

## Zobacz też:
1. [Dokumentacja biblioteki chrono](https://en.cppreference.com/w/cpp/chrono) 
2. [Poradnik porównywania dat w C++ z użyciem biblioteki Boost.DateTime](https://www.boost.org/doc/libs/1_63_0/doc/html/date_time/examples.html#date_time.examples.comp_op_eg)
3. [Poradnik jak reprezentować daty i godziny w C++](https://howardhinnant.github.io/date_algorithms.html)