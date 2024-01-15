---
title:                "Розбір HTML"
html_title:           "Python: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Вивчення розбору HTML є корисним для веб-розробки, веб-скрапінгу та автоматизації задач, пов'язаних з вебом.

## Як

```Python
from bs4 import BeautifulSoup
import requests

# Запит на веб-сторінку
url = "https://www.example.com"
response = requests.get(url)

# Створення об'єкту BeautifulSoup для роботи з HTML
soup = BeautifulSoup(response.text, 'html.parser')

# Пошук елементів за тегом або класом
titles = soup.find_all('h1')
paragraphs = soup.find_all(class_='description')

# Виведення знайдених елементів
print(titles)
print(paragraphs)
```

Результат:
```Python
[<h1>Hello, world!</h1>, <h1>Welcome to my website</h1>]
[<p class="description">This is a sample website for demonstration purposes only.</p>]
```

## Deep Dive

Один з основних інструментів для розбору HTML в Python - це бібліотека BeautifulSoup. Вона дозволяє забезпечити зручний доступ до різних елементів веб-сторінки за допомогою зручних методів. Крім того, дана бібліотека підтримує парсинг за допомогою CSS селекторів, що дозволяє швидко і ефективно отримувати потрібні дані з веб-сторінок.

## Дивитися Також
- [Документація BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Розбір HTML з Python за допомогою BeautifulSoup](https://realpython.com/beautiful-soup-web-scraper-python/)
- [Навчальне відео по розбору HTML в Python](https://www.youtube.com/watch?v=xm_kmudXFWI)