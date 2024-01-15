---
title:                "Надсилання http-запиту"
html_title:           "Python: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Для чого

Відправка HTTP-запиту є необхідним елементом у багатьох веб-застосунках, таких як API-інтеграції, веб-скрапінг і тестування. Це дозволяє отримати інформацію з іншого сервера, виконати дію або передати дані.

## Як відправити HTTP-запит використовуючи Python

Вам знадобиться встановити бібліотеку `requests` за допомогою `pip install requests` перед початком роботи з HTTP-запитами. Потім ви можете використовувати `requests.get()` для отримання вмісту сторінки із заданим URL.

```Python
import requests

response = requests.get("https://www.samplewebsite.com")
print(response.status_code)
print(response.text)
```

Очікуваний результат повинен бути код статусу 200, що означає успішне отримання вмісту сторінки. Ви також можете передати параметри, заголовки і кукіз при виконанні запиту.

## Глибша розробка

Для надійності ви можете перевірити статус поверненого коду за допомогою `response.ok` та вивести текст помилки, якщо запит не був успішним. Щоб передати дані або виконати інший тип запиту, ви можете використовувати `requests.post()` або `requests.put()`.

Підтримується також і авторизація на сайтах, де ви можете передати логін та пароль у вигляді словника `{ 'username': ..., 'password': ... }`. Для виконання запиту з файлом ви можете використовувати `requests.post()` і передати файл за допомогою `files={ 'file': open('filename.txt', 'rb') }`.

## Дивіться також

- [Документація з `requests`](https://requests.readthedocs.io/en/master/)
- [Розділ з `requests` у офіційному пайтоновському підручнику](https://docs.python.org/3/library/urllib.html)
- [Туторіал по відправці HTTP-запитів з Python](https://realpython.com/python-requests/)