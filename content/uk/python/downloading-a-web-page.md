---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що та навіщо? 

Завантаження веб-сторінки - це процес отримання його вмісту (HTML, CSS, JavaScript тощо) з сервера. Програмісти це роблять, щоб аналізувати контент, збирати дані, перевіряти наявність або відсутність певної інформації та навіть вдосконалювати веб-скрапінг.

## Як це зробити:

Python має вбудований модуль `requests` для виконання HTTP-запитів. Ось як завантажити веб-сторінку:

```Python
import requests

response = requests.get('https://example.com/')
print(response.text)
```

Вищенаведений код зверне увагу на веб-сторінку (`https://example.com/`) і виведе вміст HTML.

## Поглиблений розбір:

Завантаження веб-сторінок було ключовим аспектом взаємодії з Інтернетом з самого його початку. Але технології, які програмісти використовують до цього, з часом розвивалися.

Білий Так, у минулому більш поширеним була бібліотека Python - urllib, але через її складний API вона пізніше була замінена на requests. Існують альтернативи, як BeautifulSoup і Scrapy, але requests є найпростішим і найбільш безпосереднім засобом для цього.

За рахунок реалізації, `requests.get()` виконує HTTP GET запит до заданої URL. Він потім повертає файл відповіді, який містить статус відповіді, заголовки та вміст сторінки.

## Дивись також:

1. [Документація Python requests](https://docs.python-requests.org/)
2. [Суп з BeautifulSoup: Просте веб-скрапінг у Python](https://realpython.com/beautiful-soup-web-scraper-python/)
3. [Поглиблений погляд на HTTP у Python з Requests](https://realpython.com/python-requests/)