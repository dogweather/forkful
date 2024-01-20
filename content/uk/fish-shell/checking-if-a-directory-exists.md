---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:56:18.184629-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і Чому?
Перевірка існування каталогу дозволяє уникнути помилок під час операцій з файлами. Програмісти роблять це, щоб забезпечити безпечне читання чи запис у відповідні каталоги.

## Як це зробити:
Перевірка через команду `test`, використання `-d` для каталогів:

```Fish Shell
if test -d /шлях/до/вашого/каталогу
    echo "Каталог існує!"
else
    echo "Каталог не знайдено."
end
```

Приклад виконання:

```Fish Shell
Каталог існує!
```

Або:

```Fish Shell
Каталог не знайдено.
```

## Поглиблено:
Довідка `test -d` є стандартною командою UNIX, яка використовувалась задовго до появи Fish Shell. Альтернативи в інших оболонках - `if [ -d ... ]` для bash або `Test-Path` у PowerShell для Windows. Важливо, що Fish не використовує квадратних дужок для тестування, це робить його синтаксис чистішим і зрозумілішим. Під капотом, перевірка існування каталогу - це системний виклик, що бере метадані файлу, включно з типом (файл, каталог, посилання).

## Також дивіться:
- Офіційна документація Fish Shell (`man test` у Fish Shell): https://fishshell.com/docs/current/commands.html#test
- Інформація про файлову систему UNIX і системні виклики: https://en.wikipedia.org/wiki/Unix_filesystem
- Посібник по роботі з файловою системою в Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_filesystem