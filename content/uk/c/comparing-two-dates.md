---
title:                "C: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні часто потрібно порівнювати дати, щоб знайти різницю між ними або визначити, яка з них була раніше. Це допомагає у здійсненні різних обчислень, роботі з даними та управлінні часом в програмах.

## Як

Для порівняння двох дат у C потрібно використати функцію difftime. Наприклад, якщо ми хочемо знайти різницю в днях між двома датами:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // створюємо структури для двох дат
    struct tm first_date = {0};
    struct tm second_date = {0};

    // заповнюємо дані про дати
    first_date.tm_year = 2021 - 1900; // рік 2021
    first_date.tm_mon = 8; // вересень
    first_date.tm_mday = 5; // п'яте число

    second_date.tm_year = 2021 - 1900; // рік 2021
    second_date.tm_mon = 8; // вересень
    second_date.tm_mday = 10; // десяте число

    // порівнюємо дві дати
    double difference = difftime(mktime(&second_date), mktime(&first_date));

    // розділяемо різницю на дні
    int days = difference / (24 * 3600);

    // виводимо результат
    printf("Різниця в днях: %d", days);

    return 0;
}

```

Вивід програми буде: "Різниця в днях: 5".

## Глибокий занурення

Функція difftime повертає різницю між двома часовими значеннями у секундах. Для виконання різних обчислень, наприклад, знаходження різниці у годинах або хвилинах, потрібно використовувати додаткові перетворення та обчислення.

Також варто знати, що в структурі для представлення дати в C значення року потрібно вказувати як "рік - 1900". Наприклад, для року 2021 буде 2021-1900 = 121. Це пов'язано з історичними причинами та збереженням сумісності зі старішими версіями мови.

## Дивіться також

- [Функція difftime в документації C](https://en.cppreference.com/w/c/chrono/difftime)
- [Робота з датами та часом у C](https://www.programiz.com/c-programming/c-date-time)
- [Програмування для початківців: використання функцій у C](https://www.bogotobogo.com/CProgramming/programing_functions.php)