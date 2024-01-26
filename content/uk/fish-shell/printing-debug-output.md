---
title:                "Виведення налагоджувальної інформації"
date:                  2024-01-20T17:52:46.387345-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Вивід дебаг-інформації допомагає відслідковувати, що відбувається під капотом сценарію. Програмісти роблять це, аби швидше знаходити й виправляти помилки.

## Як це зробити:
Щоб додати вивід для відладки у Fish, використовуйте команду `echo` або `printf`. Ось як це працює:

```Fish Shell
# Виведення простого тексту
echo "Це дебаг-повідомлення"

# Форматований вивід
set var "world"
printf "Привіт, %s\n" $var

# Зберігання дебаг-інформації у файл
echo "Початок виконання скрипта" > debug.log
```

Приклад виводу:
```
Це дебаг-повідомлення
Привіт, world
```

## Глибоке занурення
Раніше для дебагінгу використовувалися журнали подій та інші засоби, але простий вивід тексту завжди був одним з найшвидших методів отримати інформацію. У Fish, на відміну від інших оболонок, немає вбудованих засобів для рівнів логування, але для цих цілей можна використати зовнішні утиліти як `logger`. При виведенні дебаг-інформації у файл, зручно використовувати перенаправлення вводу/виводу. Ретельно обираючи повідомлення для дебагінгу, можна зменшити обсяг логів та час, потрібний для аналізу.

## Дивись також:
- Документація Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Роз'яснення команди `printf`: [https://fishshell.com/docs/current/cmds/printf.html](https://fishshell.com/docs/current/cmds/printf.html)
- Ведення журналу повідомлень: [https://en.wikipedia.org/wiki/Syslog](https://en.wikipedia.org/wiki/Syslog)
