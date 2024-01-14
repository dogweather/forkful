---
title:                "C++: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Для багатьох програм, які працюють з даними про дати, порівняння двох дат є дуже корисною функцією. Вона дозволяє визначити, яка з дат більша або менша, а також визначити різницю між ними. В цьому пості ми розглянемо, як порівнювати дати в C++ і яку користь це може принести вам у вашій програмі.

## Як

У C++, звичайно, є декілька способів порівняти дві дати. Одним з найбільш зручних способів є використання стандартної бібліотеки <ctime>. Давайте подивимось на кілька прикладів коду, щоб краще розібратись в цьому.

```C++
#include <ctime>
#include <iostream>

int main() {
  // створюємо першу дату
  struct tm first_date;
  first_date.tm_year = 2020 - 1900; // рік від 1900
  first_date.tm_mon = 9; // жовтень
  first_date.tm_mday = 5; // 5-те число
  first_date.tm_hour = 0; // 0 годин
  first_date.tm_min = 0; // 0 хвилин
  first_date.tm_sec = 0; // 0 секунд

  // створюємо другу дату
  struct tm second_date;
  second_date.tm_year = 2020 - 1900; // рік від 1900
  second_date.tm_mon = 10; // листопад
  second_date.tm_mday = 3; // 3-те число
  second_date.tm_hour = 0; // 0 годин
  second_date.tm_min = 0; // 0 хвилин
  second_date.tm_sec = 0; // 0 секунд

  // перетворюємо дати в часові мітки
  time_t first_date_time = mktime(&first_date);
  time_t second_date_time = mktime(&second_date);

  // порівнюємо часові мітки
  if (first_date_time < second_date_time) {
      std::cout << "Перша дата перед другою датою" << std::endl;
  } else if (first_date_time > second_date_time) {
      std::cout << "Перша дата після другої дати" << std::endl;
  } else {
      std::cout << "Обидві дати рівні" << std::endl;
  }

  // обчислюємо різницю в днях
  double difference_in_days = difftime(second_date_time, first_date_time) / (60 * 60 * 24);
  std::cout << "Різниця між датами в днях: " << difference_in_days << std::endl;

  return 0;
}
```

В результаті ми отримаємо наступне виведення:

```
Перша дата перед другою датою
Різниця між датами в днях: 29
```

Як бачите, ми можемо визначати, яка з дат більша чи менша, а також обчислювати різницю між ними.

## Deep Dive

Але чому в C++ ми використовуємо вираз `(2020 - 1900)` для визначення року? Це пов'язано