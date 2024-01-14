---
title:                "C++: Перетворення дати у рядок"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Конвертування дати в рядок є важливою частиною програмування з мовою C++. Це дозволяє нам легко відображати дату у форматі, зрозумілому для користувачів, та пов'язувати її з іншими даними.

## Як це зробити

```C++
#include <iostream>
#include <ctime>
#include <string>

int main() {

  // Отримуємо поточну дату та час
  time_t now = time(0);

  // Конвертуємо дату у рядок
  char* convertedDate = ctime(&now);

  // Виводимо результат
  std::cout << "Поточна дата та час: " << convertedDate << std::endl;

  return 0;
}

```

Приклад виводу:

```C++
Поточна дата та час: Sat, Nov 28 10:05:00 2020
```

## Поглиблене вивчення

При конвертуванні дати в рядок використовується функція `ctime`. Вона приймає в якості аргументу вказівник на об'єкт `time_t` та повертає рядок з датою та часом. Однак, цей метод не завжди дає бажаний результат, оскільки формат рядка може бути різним залежно від операційної системи та локалізації.

Іншою альтернативою є використання функції `strftime`, яка дозволяє дещо більш гнучко вказувати формат рядка, в який буде конвертована дата.

## Додаткові матеріали

- [Посібник з праці з датою та часом у C++](https://developer.ibm.com/technologies/systems/articles/au-datetime/)
- [Документація з функції `ctime`](http://www.cplusplus.com/reference/ctime/ctime/)
- [Документація з функції `strftime`](http://www.cplusplus.com/reference/ctime/strftime/)

## Дивіться також

- [Розділ про роботу з датою та часом у курсі "Навчання за допомогою C++"](https://www.learncpp.com/cpp-tutorial/513-timing-your-code/)
- [Використання різноманітних форматів дати в C++](https://stackoverflow.com/questions/2022664/c-format-datetime)
- [Огляд функції `strftime`](https://www.tutorialspoint.com/c_standard_library/time_h.htm)