---
title:                "Надсилання HTTP запиту"
html_title:           "Python: Надсилання HTTP запиту"
simple_title:         "Надсилання HTTP запиту"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і для чого?
Відправляння HTTP-запиту - це коли комп'ютер запитує веб-сервер про веб-сторінку або інший ресурс. Це необхідно для отримання відповіді та взаємодії з іншими веб-застосунками, такими як API. Програмісти часто використовують відправку HTTP-запитів, щоб отримувати дані для своїх програм або розробляти веб-сервіси.

## Як відправити HTTP-запит?
Нижче показано приклад коду Python для відправлення GET-запиту до веб-сервера та отримання відповіді:

```python
import requests

response = requests.get('https://www.example.com')
print(response.text)
```

Після запуску цього коду, ви отримаєте відповідь від веб-сервера, яку потім можна обробити у своїй програмі.

## Глибшого погляду
HTTP (Hypertext Transfer Protocol) був розроблений в 1991 році Тімом Бернерс-Лі для передачі даних через Інтернет. Зараз існують інші альтернативи відправки запитів, такі як WebSocket і MQTT, але HTTP залишається основним протоколом для взаємодії з веб-сторінками та API. В середині кожного HTTP-запиту є заголовки, які містять метадані про запит та тіло, яке містить корисну інформацію.

## Дивись також
- [Офіційна документація Requests](https://requests.readthedocs.io/en/master/)
- [Відеоурок про відправлення HTTP-запитів в Python](https://www.youtube.com/watch?v=tb8gHvYlCFs)
- [Стаття про використання API за допомогою відправки HTTP-запитів](https://realpython.com/api-integration-in-python/)