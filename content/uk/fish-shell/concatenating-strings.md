---
title:                "Fish Shell: З'єднання рядків"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому
Поєднання рядків є корисною технікою у програмуванні, оскільки дозволяє збирати різні частини тексту в один рядок. Це особливо корисно, коли потрібно форматувати вивід або створювати текстові повідомлення.

## Як
Продемонструємо, як поєднати рядки в Fish Shell за допомогою оператора `=` та команди `echo`.

```Fish Shell
set message "Привіт"
set name "Україно!"
echo $message$name
```

Вивід: `Привіт Україно!`

Також можна використовувати оператор `+=`, щоб додавати текст до існуючого рядка:

```Fish Shell
set message "Привіт"
set name "Україно!"
set message += " " + $name
echo $message
```

Вивід: `Привіт Україно!`

## Глибоке погруження
Крім використання операторів `=` та `+=`, також можна використовувати команду `string join`, яка дозволяє поєднувати більше ніж два рядки.

```Fish Shell
set colors "червоний " "зелений " "синій "
set result (string join $colors)
echo $result
```

Вивід: `червоний зелений синій`

## Дивись також
- [Офіційна документація Fish Shell](https://fishshell.com/docs/current/)
- [Корисні ресурси для початківців з Fish Shell](https://www.freecodecamp.org/news/fish-shell-a-beginners-guide/)
- [Швидка довідка по Fish Shell](https://medium.com/faun/fish-shell-quick-reference-6795c351d924)