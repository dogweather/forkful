---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:03.975300-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Python \u043D\u0430\u0434\u0430\u0454 \u043F\u043E\u0442\u0443\u0436\u043D\u0456\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\
  \u0456 \u044F\u043A BeautifulSoup \u0442\u0430 requests, \u0434\u043B\u044F \u0432\
  \u0435\u0431-\u0441\u043A\u0440\u0430\u043F\u0456\u043D\u0433\u0443 \u0442\u0430\
  \ \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 HTML. \u0414\u043B\u044F \u043F\
  \u043E\u0447\u0430\u0442\u043A\u0443, \u0432\u0430\u043C \u043F\u043E\u0442\u0440\
  \u0456\u0431\u043D\u043E\u2026"
lastmod: '2024-03-13T22:44:48.579710-06:00'
model: gpt-4-0125-preview
summary: "Python \u043D\u0430\u0434\u0430\u0454 \u043F\u043E\u0442\u0443\u0436\u043D\
  \u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\
  \u043A\u0456 \u044F\u043A BeautifulSoup \u0442\u0430 requests, \u0434\u043B\u044F\
  \ \u0432\u0435\u0431-\u0441\u043A\u0440\u0430\u043F\u0456\u043D\u0433\u0443 \u0442\
  \u0430 \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 HTML."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

## Як це зробити:
Python надає потужні бібліотеки, такі як BeautifulSoup та requests, для веб-скрапінгу та парсингу HTML. Для початку, вам потрібно встановити ці бібліотеки, якщо ви ще цього не зробили:

```bash
pip install beautifulsoup4 requests
```

Ось базовий приклад використання `requests` для отримання HTML-вмісту вебсторінки та `BeautifulSoup` для його парсингу:

```python
import requests
from bs4 import BeautifulSoup

# Отримати вміст вебсторінки
URL = 'https://example.com'
page = requests.get(URL)

# Парсити HTML-вміст
soup = BeautifulSoup(page.content, 'html.parser')

# Приклад вилучення заголовку вебсторінки
title = soup.find('title').text
print(f'Назва вебсторінки: {title}')
```

**Приклад виведення**:
```
Назва вебсторінки: Example Domain
```

Для більш складних запитів, як-от вилучення всіх посилань із вебсторінки, ви можете використовувати різноманітні методи BeautifulSoup для навігації та пошуку у дереві аналізу:

```python
# Вилучити всі посилання в межах тегів <a>
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**Приклад виведення**:
```
https://www.iana.org/domains/example
```

Гнучкість BeautifulSoup дозволяє налаштувати ваш пошук для точних даних, які вам потрібні, роблячи парсинг HTML потужним інструментом для програмістів, які працюють з веб-вмістом.
