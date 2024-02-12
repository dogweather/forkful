---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T17:59:33.464886-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Відправлення HTTP запиту – це спосіб "поговорити" з веб-сайтом чи сервісом. Програмісти роблять це, щоб отримати дані, відправити інформацію або керувати веб-ресурсами.

## How to: (Як це зробити:)

```Fish Shell
# Встановлення httpie для відправлення HTTP запитів
fisher install httpie

# Відправлення GET запиту
http GET example.com

# Відправлення POST запиту з даними
http POST example.com name=Ivan age=34

# Результат виконання:
# HTTP/1.1 200 OK
# ...
# {
#     "json": {
#         "age": "34",
#         "name": "Ivan"
#     }
# }
```

## Deep Dive (Поглиблений Розгляд)

Ще у ранніх днях інтернету, відправлення HTTP запитів було ключовою частиною веб-розробки. Сьогодні, зручні інструменти як `httpie` сильно спростили процес. Альтернативи `httpie` включають `curl` та бібліотеку `requests` для Python. У Fish Shell, `httpie` є інтуїтивним завдяки своїй простоті та читабельності команд. При відправленні запитів ми маємо справу з методами HTTP як GET для отримання даних та POST для відправки. Ми можемо також встановлювати заголовки запиту, передавати параметри та управляти відповідями сервера через наші команди.

## See Also (Дивіться також)

- Документація Fish Shell: https://fishshell.com/docs/current/index.html
- httpie GitHub Repository: https://github.com/httpie/httpie
- HTTP протокол: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Огляд основ HTTP запитів з `curl`: https://curl.se/docs/manual.html
