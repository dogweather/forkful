---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:44:55.598754-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Завантаження веб-сторінки – це процес отримання її вмісту через Інтернет. Програмісти роблять це заради обробки інформації, автоматизації завдань, або для збору даних.

## Як це зробити:
Ось як можна завантажити вміст веб-сторінки у Python за допомогою модуля `requests`.

```Python
import requests

# Надсилаємо GET-запит до веб-сторінки
response = requests.get('https://example.com')

# Переконуємося, що запит був успішним
if response.ok:
    # Виводимо вміст сторінки
    print(response.text)
```

Якщо запустити код, ви побачите HTML веб-сторінки example.com в консолі.

## Поглиблений Розгляд:

У минулому для завантаження вмісту веб-сторінки використовувалися такі інструменти, як `urllib`. Проте модуль `requests` став золотим стандартом через свою простоту та зручність.

Альтернативою `requests` може бути використання `http.client` або зовнішніх інструментів, таких як Curl в командному рядку. Для парсингу HTML часто використовується `BeautifulSoup`, а для JavaScript-важких сторінок - `Selenium`.

Внутрішньо, завантаження сторінки робить HTTP або HTTPS запит. `GET` запити використовуються для отримання вмісту, тоді як `POST` і інші типи запитів для відправлення даних.

## Дивіться також:

- Документація модулю requests: https://requests.kennethreitz.org/en/master/
- BeautifulSoup документація: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Selenium документація: https://selenium-python.readthedocs.io/
- Руководство по Curl: https://curl.se/docs/manual.html
