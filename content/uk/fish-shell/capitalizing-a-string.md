---
title:                "Капіталізація рядка"
html_title:           "Fish Shell: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Запрограмувати у версії Fish Shell у стилі мови розмовного спілкування - це круто! Але якщо ти задумуєшся про те, чому потрібно капіталізувати рядок? Ось декілька прикладів використання цієї функції.

## Як

```Fish Shell
set my_str "привіт світ!"
capitalize $my_str
```
Виведе: "Привіт світ!"

```Fish Shell
set filename "файл.txt"
capitalize -t $filename
```
Виведе: "Файл.txt"

## Глибокий занурення

У версії Fish Shell є функція capitalize, яка дозволяє капіталізувати перший символ рядка. Якщо передати параметр -t, то капіталізується кожне слово в рядку. Також можна використовувати цю функцію для капіталізації назви файлу, що може бути корисно при роботі з файловою системою.

## Дивись також

- [Документація Fish Shell по capitalize](https://fishshell.com/docs/current/cmds/capitalize.html)
- [Стаття на сайті Fish Shell про capitalize](https://fishshell.com/docs/current/tutorial.html#capitalize)