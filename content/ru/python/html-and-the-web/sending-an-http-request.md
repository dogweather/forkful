---
title:                "Отправка HTTP-запроса"
aliases: - /ru/python/sending-an-http-request.md
date:                  2024-01-29T00:02:37.254928-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса – это способ, которым ваш код запрашивает у другой системы данные или услуги через Интернет. Программисты делают это для взаимодействия с веб-API, получения веб-контента или общения с другими серверами.

## Как:

Внешняя библиотека `requests` для Python делает выполнение HTTP-вызовов простым. Ниже приведен пример отправки простого GET-запроса:

```python
import requests

response = requests.get('https://api.example.com/data')
print(response.status_code)  # Выводит код состояния ответа
print(response.json())      # Если ответ содержит JSON, выводит его как словарь Python
```

Более подробный POST-запрос с JSON-полезной нагрузкой и пользовательскими заголовками:

```python
import requests
import json

url = "https://api.example.com/submit"
data = {'key': 'value'}
headers = {'Content-Type': 'application/json'}

response = requests.post(url, data=json.dumps(data), headers=headers)

print(response.status_code)
print(response.json())
```

## Подробное изучение

HTTP-запросы - это то, как работает веб - они существуют с начала 90-х. Альтернативы библиотеке `requests` Python включают стандартную библиотеку `urllib`, но она немного более громоздка.

Понимание отправки HTTP-запросов включает в себя знание методов (GET, POST, PUT, DELETE и др.), кодов состояния (например, 200 OK, 404 Not Found), заголовков и тела данных.

Для потоковых или асинхронных запросов вы можете исследовать асинхронный аналог `requests` или пакет `aiohttp`. Под капотом эти библиотеки используют `socket` Python для непосредственного сетевого общения.

Исторически `requests` считается идиальным выбором из-за его простоты и мощности, но `httpx`, более новая библиотека, совместимая с асинхроным программированием, набирает популярность.

## Смотрите также

- Документация библиотеки `requests`: https://requests.readthedocs.io
- Объяснение кодов состояния HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- Документация Python `urllib`: https://docs.python.org/3/library/urllib.html
- Библиотека `httpx` для асинхронных HTTP-запросов: https://www.python-httpx.org
