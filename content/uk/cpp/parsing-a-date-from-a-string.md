---
title:                "Розбір дати з рядка"
html_title:           "C++: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що & Чому?
Написання програмного коду, який може виділяти дату з рядка, є важливою задачею для багатьох програмістів. Це дозволяє зчитувати та обробляти дати, які зберігаються у вигляді тексту, забезпечуючи більш зручний і ефективний спосіб роботи з даними.

## Як це зробити:
```C++
#include <iostream>
#include <string>
#include <sstream>
#include <ctime>

int main()
{
  // задаємо рядок з датою у форматі dd/mm/yyyy
  std::string date = "21/10/2021";

  // створюємо потік для зчитування даних
  std::istringstream ss(date);

  // задаємо формат дати
  std::tm t = {};

  // виконуємо розбір дати з рядка
  ss >> std::get_time(&t, "%d/%m/%Y");

  // отримуємо об'єкт дати для подальшої обробки
  std::time_t time = std::mktime(&t);

  // виводимо результат у форматі timestamp
  std::cout << "Timestamp: " << time << std::endl;

  return 0;
}
```

Вивід:
```
Timestamp: 1634774400
```

## Глибокий занурення:
Перед тим, як існувало стандартне засіб ```std::get_time```, програмісти використовували інші методи для розбору дат з рядків. Наприклад, функція ```atoi``` перетворювала рядок у ціле число, що потім могло бути використане для організації дати. Також, наявність різноманітних форматів дат і універсальних засобів для їх розбору, наприклад, бібліотека Boost, спрощує завдання роботи з датами.

## Дивись також:
- [cppreference.com - std::get_time](https://en.cppreference.com/w/cpp/io/manip/get_time)
- [GeeksforGeeks - Parsing a CSV file in C++](https://www.geeksforgeeks.org/parsing-csv-files-using-c-programming/)
- [Boost.Date_time](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)