---
title:                "Знаходження довжини рядка"
html_title:           "Fish Shell: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Знаходження довжини рядка є важливою функцією при програмуванні, оскільки дозволяє визначити кількість символів у рядку і використовувати цю інформацію для подальших обчислень або маніпуляцій з даними.

## Як це зробити

```Fish Shell
function string_length
  set str "Hello world!" #Замінити на потрібний рядок
  echo (count $str) #Виведе довжину рядка
end
```

Результат:

```
![](https://github.com/fish-shell/fish-shell/raw/master/share/doc/images/string_length.png)
```

## Глибока знарядка

Функція `count` використовується для знаходження кількості символів у рядку. Вона повертає натуральне число, яке дорівнює кількості символів у заданому рядку. Також можна обробити більше одного рядка за один раз, передаючи їх як аргументи до функції `count`.

## Дивіться також 

- [Офіційна документація Fish Shell] (https://fishshell.com/docs/current/index.html#counting-characters)
- [Стаття про знаходження довжини строки у Bash Shell] (https://linuxize.com/post/bash-string-length/)