---
title:                "Аналіз html"
html_title:           "Bash: Аналіз html"
simple_title:         "Аналіз html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Парсинг HTML є важливою навичкою для програмістів, які працюють зі веб-додатками. Він дозволяє отримувати та обробляти дані з веб-сторінок, що дозволяє робити різноманітні речі, наприклад, створювати збірники даних або автоматично заповнювати форми.

## Як

Для початку, необхідно імпортувати бібліотеку "BeautifulSoup", що дозволяє просто та ефективно парсити HTML код. Наступним кроком буде завантаження веб-сторінки, з якої ми хочемо отримати дані. Наприклад, ми хочемо отримати заголовок статті з веб-сторінки "example.com":
```Bash
from bs4 import BeautifulSoup
import requests

r = requests.get("http://example.com")
soup = BeautifulSoup(r.content, 'html.parser')

header = soup.find('h1').get_text()
```

У даному прикладі ми використовуємо метод "find" для пошуку тегу "h1" та метод "get_text", щоб отримати текст в цьому тегу. Ми можемо також отримати всі посилання на сторінці:
```Bash
links = soup.find_all('a')
for link in links:
  print(link.get('href'))
```

## Глибоке погруження

Парсинг HTML дозволяє отримувати не лише текст та посилання, але й інші дані, такі як таблиці, зображення та форми. Для цього можна використовувати різні методи "find" та "find_all" для пошуку необхідних елементів на сторінці. Також існують бібліотеки, які дозволяють помічати дані на сторінках та отримувати їх у зручному вигляді, наприклад, "Scrapy".

## Дивись також

- [BeautifulSoup documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Scrapy documentation](https://docs.scrapy.org/en/latest/)
- [Python requests library documentation](https://requests.readthedocs.io/en/latest/)