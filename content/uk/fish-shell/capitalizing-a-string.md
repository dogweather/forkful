---
title:                "Fish Shell: Перетворення рядка на великі літери"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Зараз в більшості мов програмування є вбудована функція, яка автоматично перетворює рядки на великі або малі літери. Проте в Fish Shell ця функція відсутня, тому деякі користувачі можуть хотіти вручну виконати дану операцію для певної роботи.

## Як

```Fish shell
set string "ukrainian language"
echo $string
echo (string capitalize $string) 
```
- Виведе `ukrainian language` і `Ukrainian Language` для стандартної функції capitalize.
- При використанні параметру `-w` або `--words` можна вказати які слова потрібно капіталізувати.
- Значення першої літери слова можна з переключити з `-lf` або `--leading-first` параметром.

## Deep Dive

Функція capitalize в Fish Shell робить просте перетворення рядків в іншу форму. Але деякі користувачі можуть захотіти змінити стандартну поведінку або навіть додати додаткові регулярні вирази для більш тонкого капіталізації рядків. Це може бути корисно для складних рядків, таких як назви людей або пунктуаційні символи.

На щастя, у Fish Shell є бібліотека fzf, яка містить кілька вбудованих функцій для роботи з рядками. Це дає можливість користувачам змінювати та адаптувати функцію capitalize під свої потреби.

## See Also

- [Fish Shell Website](https://fishshell.com/)
- [Fish Shell Source Code](https://github.com/fish-shell/fish-shell)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell User's Guide (Ukrainian)](https://sanstv.ru/fishbook)