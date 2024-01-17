---
title:                "Porównywanie dwóch dat"
html_title:           "C: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Czym jest porównywanie dwóch dat i dlaczego programiści to robią?
Porównywanie dwóch dat jest procesem, w którym program porównuje dwie daty i określa, która jest wcześniejsza lub późniejsza. Programiści często wykonują to w celu porównania wydarzeń lub określenia, które wydarzenie nastąpiło pierwsze.

## Jak to zrobić:
```C
#include <stdio.h>
#include <time.h>

int main()
{
  //Definiowanie dwóch dat do porównania
  struct tm date1 = { .tm_year = 2020, .tm_mon = 0, .tm_mday = 1 };
  struct tm date2 = { .tm_year = 2021, .tm_mon = 0, .tm_mday = 1 };
  
  //Porównanie dat przy użyciu funkcji difftime
  double difference = difftime(mktime(&date2), mktime(&date1));
  
  //Wyświetlenie wyników
  if (difference > 0)
  {
    printf("Data 2 jest późniejsza niż data 1\n");
  }
  else if (difference < 0)
  {
    printf("Data 1 jest późniejsza niż data 2\n");
  }
  else
  {
    printf("Obie daty są takie same\n");
  }
  
  return 0;
}
```

## Głębsza analiza:
Porównywanie dwóch dat jest powszechną czynnością w programowaniu, ponieważ często musimy określić, które wydarzenie nastąpiło wcześniej lub później. W starszych wersjach języka C, funkcja difftime nie istniała i programiści musieli używać funkcji mktime oraz wykonywać obliczenia na strukturach tm. W alternatywnym języku C++, istnieje wygodniejsza funkcja std::chrono::duration, która umożliwia proste porównywanie dwóch dat. Implementacja porównywania dwóch dat może się różnić w zależności od systemu operacyjnego, dlatego warto uważać na potencjalne problemy związane z datami w swoim kodzie.

## Zobacz także:
- [Funkcja difftime w dokumentacji języka C](https://www.cplusplus.com/reference/ctime/difftime/)
- [Porównywanie dat w języku C++](https://www.geeksforgeeks.org/c-comparison-dates-time/#:~:text=To%20compare%20two%20dates%2C%20we,both%20the%20dates%20struct%20type.&text=It%20returns%20difference%20between%20both,other%20by%20returning%20a%20negative.)
- [Implementacja dat w języku C](https://en.wikipedia.org/wiki/C_date_and_time_functions#Time_difference_calculation)