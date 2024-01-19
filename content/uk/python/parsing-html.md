---
title:                "Розбір html"
html_title:           "Javascript: Розбір html"
simple_title:         "Розбір html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/parsing-html.md"
---

{{< edit_this_page >}}

## Що це & Навіщо?

Парсинг HTML – це процес вибору специфічних даних з HTML документа. Програмісти парсять HTML, щоб автоматикувати збір та обробку веб-даних, що призводить до більш ефективної роботи.

## Як це робити:

Ось простий приклад того, як парсити HTML за допомогою бібліотеки BeautifulSoup в Python:

```Python
from bs4 import BeautifulSoup
import requests

site = requests.get("https://www.example.com")
soup = BeautifulSoup(site.content, 'html.parser')

print(soup.prettify())
```
Вищенаведений код витягує весь вихідний код веб-сторінки https://www.example.com і друкує його у відформатованому вигляді.

## Поглиблено

Усталено, що парсинг HTML почався як спосіб обробки і візуалізації HTML задокументів. Насправді, переглядачі використовують парсинг усередині, щоб обробити вихідний код HTML і відтворити його у вигляд, що ви бачите.

Але існують і альтернативи. Регулярні вирази можуть бути використані для парсингу HTML, але їхня складність і виключность змушують віддати перевагу парсерам HTML.

Деякі глибинні деталі реалізації включають те, як парсери обробляють неправильний HTML і вихідні різні об'єкти DOM.

## Дивіться також

[BeautifulSoup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)

[Python requests module](https://docs.python.org/3/library/requests.html)

Ці посилання стануть корисними, адже дають докладніше розуміння процесів, що стосуються парсингу HTML.