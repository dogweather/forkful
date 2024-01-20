---
title:                "Відправлення HTTP-запиту"
html_title:           "Bash: Відправлення HTTP-запиту"
simple_title:         "Відправлення HTTP-запиту"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Надсилання HTTP-запиту - це процес передачі конкретної інформації до сервера через Інтернет. Програмісти роблять це, щоб взаємодіяти з веб-серверами для отримання, відправлення або оновлення даних.

## Як це зробити:
В Bash можна надіслати HTTP-запит за допомогою утиліти `curl`. Ось приклад:

```Bash
curl https://example.com
```

Ви отримаєте відповідь у вигляді HTML-сторінки. Якщо ви хочете надіслати POST-запит, використовуйте `-d`:

```Bash
curl -d "param1=value1&param2=value2" -X POST http://example.com/resource.cgi
```

## Поглиблений Огляд:
Nадсилання HTTP-запитів було важливою частиною веб-розробки протягом багатьох років. CURL, створений в 1990-х, переважно використовувався для передачі даних через різні протоколи.

Альтернативи CURl - це `wget` та `httpie`, які також використовуються для роботи з HTTP. Якщо розглядати аспекти реалізації, CURL є бібліотекою з набором функцій, які можна вбудувати в свою програму для виконання HTTP-запитів.

## Дивись Також:
1. [Офіційна документація CURL](https://curl.haxx.se/docs/manpage.html)
2. [Руководство по Bash](https://www.gnu.org/software/bash/manual/)
3. [HTTPie: сучасна командна стрічка HTTP-клієнта](https://httpie.io/)