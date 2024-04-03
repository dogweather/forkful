---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:37.254928-07:00
description: "\u041A\u0430\u043A: \u0412\u043D\u0435\u0448\u043D\u044F\u044F \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430 `requests` \u0434\u043B\u044F\
  \ Python \u0434\u0435\u043B\u0430\u0435\u0442 \u0432\u044B\u043F\u043E\u043B\u043D\
  \u0435\u043D\u0438\u0435 HTTP-\u0432\u044B\u0437\u043E\u0432\u043E\u0432 \u043F\u0440\
  \u043E\u0441\u0442\u044B\u043C. \u041D\u0438\u0436\u0435 \u043F\u0440\u0438\u0432\
  \u0435\u0434\u0435\u043D \u043F\u0440\u0438\u043C\u0435\u0440 \u043E\u0442\u043F\
  \u0440\u0430\u0432\u043A\u0438 \u043F\u0440\u043E\u0441\u0442\u043E\u0433\u043E\
  \ GET-\u0437\u0430\u043F\u0440\u043E\u0441\u0430."
lastmod: '2024-03-13T22:44:44.264253-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043D\u0435\u0448\u043D\u044F\u044F \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A\u0430 `requests` \u0434\u043B\u044F Python \u0434\u0435\
  \u043B\u0430\u0435\u0442 \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u0435\
  \ HTTP-\u0432\u044B\u0437\u043E\u0432\u043E\u0432 \u043F\u0440\u043E\u0441\u0442\
  \u044B\u043C."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

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
