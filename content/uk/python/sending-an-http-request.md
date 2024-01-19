---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що та Навіщо?

Відправка HTTP-запиту - це процес обміну даними між клієнтом (вашим комп'ютером) та сервером. Програмісти це роблять, щоб отримати, відправити, оновити або видалити інформацію на сервері.

## Як це робити:

Використовуємо бібліотеку Python `requests`. Поглянемо на приклади коду:

```python
import requests

# Створюємо GET запит
response = requests.get('http://example.com')

# Виведемо статус-код відповіді
print(response.status_code)

# Виведемо вміст відповіді
print(response.text)
```

Якщо все в порядку, ви побачите код статусу `200`, що означає "ОК", і вміст HTML-сторінки example.com.

## Поглиблений аналіз:

**Історичний контекст**: HTTP запити були в основі веб-браузерного спілкування з серверами з моменту зародження WWW у 1990-х.

**Альтернативи**: Інші популярні бібліотеки для роботи з HTTP в Python це `urllib`, `httplib`, але `requests` є набагато простішою та зручнішою.

**Деталі реалізації**: Бібліотека `requests` використовує методи `get`, `post`, `put`, `delete` тощо, які відповідають однойменним методам HTTP. Ви також можете додати параметри запиту, заголовки та дані форми в свої запити.

## Дивіться також:

1. Офіційна документація `requests`: http://docs.python-requests.org/
2. Введення в HTTP: https://developer.mozilla.org/uk/docs/Web/HTTP/Overview
3. Підручник Python `requests`: https://realpython.com/python-requests/