---
title:                "Перевірка наявності директорії"
html_title:           "Fish Shell: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і чому?

Перевірка, чи існує директорія – це процесс визначення, чи фактично директорія існує на файловій системі. Програмісти роблять це, щоб уникнути помилок при читанні/запису файлоів.


## Як це зробити:

```fish
if test -d /шлях/до/вашої/директорії
    echo "Директорія існує"
else
    echo "Директорія не існує"
end
```

Якщо директорія існує, виведе "Директорія існує", інакше виведе "Директорія не існує".

## Поглиблений огляд:

Цей підхід до перевірки існування директорії вже довгий час є стандартом в Unix-подібних системах. Існують інші методи, але вони можуть бути більш складними або менш надійними. 'test -d' працює, перевіряючи, чи існує інформація про директорію в файловій системі.

## Дивіться також:

1. [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
2. [Filesystem Testing with Unix](https://www.tldp.org/LDP/abs/html/fto.html)
3. [Fish Shell GitHub Repository](https://github.com/fish-shell/fish-shell)