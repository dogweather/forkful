---
title:                "Аналіз HTML"
aliases: - /uk/python/parsing-html.md
date:                  2024-02-03T19:13:03.975300-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Парсинг HTML полягає у аналізі HTML-коду вебсторінки для вилучення специфічної інформації або елементів, що є поширеною задачею для веб-скрапінгу, майнінгу даних або автоматизації взаємодії з вебсайтами. Програмісти роблять це для програмного взаємодії з вебсайтами або вилучення даних із них, автоматизації задач або тестування веб-аплікацій.

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
