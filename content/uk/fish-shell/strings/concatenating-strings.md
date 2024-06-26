---
date: 2024-01-20 17:34:55.268548-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0421\
  \u043A\u043B\u0435\u044E\u0432\u0430\u043D\u043D\u044F \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u0432 Fish \u043C\u043E\u0436\u043D\u0430 \u0437\u0440\u043E\u0431\u0438\
  \u0442\u0438 \u043F\u0440\u043E\u0441\u0442\u043E \u043F\u043E\u0441\u0442\u0430\
  \u0432\u0438\u0432\u0448\u0438 \u0457\u0445 \u043F\u043E\u0440\u044F\u0434."
lastmod: '2024-03-13T22:44:50.050736-06:00'
model: gpt-4-1106-preview
summary: "\u0421\u043A\u043B\u0435\u044E\u0432\u0430\u043D\u043D\u044F \u0440\u044F\
  \u0434\u043A\u0456\u0432 \u0432 Fish \u043C\u043E\u0436\u043D\u0430 \u0437\u0440\
  \u043E\u0431\u0438\u0442\u0438 \u043F\u0440\u043E\u0441\u0442\u043E \u043F\u043E\
  \u0441\u0442\u0430\u0432\u0438\u0432\u0448\u0438 \u0457\u0445 \u043F\u043E\u0440\
  \u044F\u0434."
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
weight: 3
---

## Як це робити:
Склеювання рядків в Fish можна зробити просто поставивши їх поряд.

```Fish Shell
set greeting "Привіт, "
set name "світе!"
echo $greeting$name
```
Вивід:
```
Привіт, світе!
```

Ще можна використовувати команду `string`:

```Fish Shell
set full (string join '' $greeting $name)
echo $full
```
Вивід:
```
Привіт, світе!
```

## Поглиблено:
У більшості командних оболонок основний механізм склеювання рядків — це просто писати їх поруч. У минулому цей процес міг бути менш інтуїтивним або вимагати спеціальних інструментів.

Fish Shell відрізняється спрощеністю. Використовуючи лише пробіли для об'єднання рядків, дозволяє коду бути чистим і читабельним. Крім того, команда `string` пропонує розширені можливості для маніпулювання рядками.

Альтернативи в інших оболонках включають оператори склеювання, такі як '+' у PowerShell або '.' у PHP. Однак, в Bash і POSIX шелах зазвичай рядки склеюються без прямих операторів.

## Дивіться також:
- Офіційна документація по Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- String manipulation in Fish: [https://fishshell.com/docs/current/commands.html#string](https://fishshell.com/docs/current/commands.html#string)
- Stack Overflow - questions and answers about Fish Shell: [https://stackoverflow.com/questions/tagged/fish](https://stackoverflow.com/questions/tagged/fish)
