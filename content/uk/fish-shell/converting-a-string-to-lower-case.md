---
title:                "Перетворення рядка в нижній регістр."
html_title:           "Fish Shell: Перетворення рядка в нижній регістр."
simple_title:         "Перетворення рядка в нижній регістр."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Існує багато випадків, коли нам потрібно перетворити рядок на різні регістри. Наприклад, для зручності порівняння, пошуку або виведення. Конвертація рядка до нижнього регістру є одним із способів цього досягнути. Давайте подивимось, як ми можемо зробити це в Fish Shell.

## Як

```Fish Shell
set string "UpPerCasE STRiNg"
echo $string | tr '[:upper:]' '[:lower:]'
```

Використовуючи вбудований інструмент `tr`, ми можемо перетворити будь-який рядок до нижнього регістру. Просто використовуйте `echo` для введення рядка та вказати, які символи слід перетворити до нижнього регістру.

__Виведення:__
```Fish Shell
uppercase string
```

Також існує інший шлях за допомогою розділу `lowercase` в Fish Shell:

```Fish Shell
set string "HeLlo!"
string | lowercase # The "'" character is used to split multiple commands on one line.
```

__Виведення:__
```Fish Shell
hello!
```

## Глибинне дослідження

Як ми бачимо, конвертація рядка до нижнього регістру на Fish Shell є досить простою. Але що відбувається під капотом?

Перш за все, для перетворення регістру ми використовуємо інструмент `tr`, який є частиною стандартних UNIX-утиліт. Він приймає два аргументи - символи з верхнім та нижнім регістрами і заміняє їх у рядку.

У нашому прикладі, ми передаємо `[:upper:]` та `[:lower:]` як аргументи `tr`, що означає всі символи з верхнім регістром будуть замінені на символи з нижнім регістром. Це надійний та швидкий спосіб застосування конвертації регістру до будь-якого рядка у Fish Shell.

## Дивіться також

- [Офіційна документація Fish Shell](https://fishshell.com/docs/current/index.html)
- [Інші корисні статті про Fish Shell](https://fishshell.com/docs/current/index.html#tutorials)