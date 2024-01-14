---
title:                "Fish Shell: Перетворення рядка у нижній регістр"
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Перетворення рядка на малі символи є важливим аспектом програмування в оболонці Fish Shell. Це дозволяє зробити дані більш однорідними та звести випадкові помилки до мінімуму.

## Як це зробити

```Fish Shell
set my_variable "Hello WORLD"
echo $my_variable
``` 
Вивід: "Hello WORLD"

```Fish Shell
set my_variable (string tolower $my_variable)
echo $my_variable
```
Вивід: "hello world"

## Глибоке дослідження

Функція `string tolower` використовується для конвертації рядка в нижній регістр. Це означає, що всі великі літери будуть замінені на малі. Також, прикладаючи функцію до змінної, ми можемо змінити її значення безпосередньо у коді. Це особливо корисно при роботі з інпутами користувача, де ми не можемо гарантувати, що дані будуть введені у правильному форматі.

## Дивись також

- [Документація Fish Shell](https://fishshell.com/docs/current/)
- [Конвертація рядка в верхній регістр](https://fishshell.com/docs/current/cmds/toupper.html)
- [Робота з рядками в Fish Shell](https://fishshell.com/docs/current/tutorial.html#strings)