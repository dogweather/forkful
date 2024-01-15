---
title:                "Отримання поточної дати"
html_title:           "C: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Для чого

Програмування - це складний процес, який вимагає багато уваги до деталей. Одним із перших кроків у програмуванні є здобуття поточної дати, яка є важливою для багатьох програм. Вона може бути використана для створення звітів, організації розкладів або у бізнес-логіці програми.

## Як

За допомогою функції `time()` з бібліотеки `time.h` ми можемо отримати поточну дату. Нижче подано приклад коду та його виводу:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // отримуємо поточний час
    time_t t = time(NULL); 

    // перетворюємо час у необхідний формат
    struct tm *current_time = localtime(&t); 

    // відображаємо поточну дату та час
    printf("Поточна дата та час: %s\n", asctime(current_time)); 

    return 0;
}
```

Вивід:

```
Поточна дата та час: <назва місяця> <номер дня> <час> <часовий пояс> <<назва року>>
```

## Глибоке занурення

`time()` - це стандартна функція, яка повертає поточний час в секундах з 1 січня 1970 року. Для зручності, її значення можна перетворити у звичайний формат дати або використати для обчислення часу в програмі. Детальніше про роботу з датою і часом можна дізнатися у документації [C time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm).

## Дивіться також

- [Стаття "Як здобути поточну дату в C" на сайті Programiz](https://www.programiz.com/c-programming/library-function/time)
- [Огляд функції `time()` у статті "Повертаємо час" на сайті журналу "Наука в фокусі"](https://focus.ua/science/283094-nauka_fokus_properties_time)
- [Детальніше про функцію `localtime()` на сайті geeksforgeeks](https://www.geeksforgeeks.org/c-program-to-get-time/)