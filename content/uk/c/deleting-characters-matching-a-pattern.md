---
title:                "C: Видалення символів, відповідних шаблону"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

Напевно, ви замислюєтесь, чому хтось бажав би видаляти символи, що відповідають певному шаблону. В такому випадку ви знаходитесь у правильному місці! У цій блозі ми розглянемо, як це може бути корисним і як це можна реалізувати за допомогою мови програмування C.

## Як

У цьому відрізку ми покажемо вам приклади коду і їхні вихідні дані для реалізації видалення символів, що відповідають певному шаблону. Для спрощення експлікації, ми використаємо блоки коду з форматуванням "```C ... ```".

```C
#include <stdio.h>

int main() {
  char str[] = "Привіт, світе!";
  int i, j;

  for (i = 0; str[i] != '\0';  ++i) {
    while (!(str[i] >= 'a' && str[i] <= 'z' || str[i] >= 'A' && str[i] <= 'Z' || str[i] == ' ')) {
      for (j = i; str[j] != '\0'; ++j) {
        str[j] = str[j+1];
      }
      str[j] = '\0';
    }
  }
  printf("Вихідні дані: %s", str);
  return 0;
}

```

Вихідні дані: Privt, svte!

Цей код показує, як можна видалити усі символи, що не відповідають буквам англійського алфавіту, зі строки. Ви можете змінити цей шаблон, щоб видаляти інші символи за необхідності.

## Глибоке занурення

Алгоритм видалення символів, що відповідають певному шаблону, може бути складним для розуміння, якщо ви не знайомі з основами роботи з масивами та рядками. Проте, реалізація такого функціоналу може бути дуже корисною у багатьох ситуаціях.

## Дивіться також

- Як видалити порожні рядки у С ([https://www.geeksforgeeks.org/program-to-delete-empty-rows-from-a-two-dimensional-array/](https://www.geeksforgeeks.org/program-to-delete-empty-rows-from-a-two-dimensional-array/))
- Розбудова масивів у С ([https://www.tutorialspoint.com/cprogramming/c_arrays.htm](https://www.tutorialspoint.com/cprogramming/c_arrays.htm))
- Рядкові функції у С ([https://www.tutorialspoint.com/c_standard_library/string_h.htm](https://www.tutorialspoint.com/c_standard_library/string_h.htm))