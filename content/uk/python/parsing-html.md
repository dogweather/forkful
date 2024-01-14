---
title:                "Python: Аналіз html."
simple_title:         "Аналіз html."
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Зараз насамперед користувачі нашого сайту, скоріш за все, цікавимуться тим, чому вони повинні вчитися парсити HTML. На початку може здатися, що це складний процес, але насправді, це корисний і необхідний навичка для всіх програмістів.

## Як це зробити

Нижче наведені приклади коду Python для парсингу HTML та відображення результату:

```Python
# імпортуємо необхідні бібліотеки
from bs4 import BeautifulSoup
import requests

# створюємо з'єднання з сторінкою
url = "https://www.example.com"
page = requests.get(url)

# отримуємо HTML-код сторінки
soup = BeautifulSoup(page.content, 'html.parser')

# шукаємо всі елементи <h1> та відображаємо їх текст
headings = soup.find_all('h1')
for heading in headings:
    print(heading.get_text())

# шукаємо всі посилання та відображаємо їх URL
links = soup.find_all('a')
for link in links:
    print(link['href'])
```

Запустивши цей код, ви отримаєте заголовки <h1> та посилання, які знаходяться на сторінці "https://www.example.com".

## Глибше погрузження

Парсинг HTML - це процес структуризації та отримання даних з HTML-коду сторінки. Для цього використовуються спеціальні бібліотеки, такі як BeautifulSoup або lxml. Парсинг HTML корисний для отримання інформації з веб-сторінок для подальшого використання або аналізу даних.

Є багато різних методів та підходів до парсингу HTML, тому рекомендуємо докладно ознайомитися з документацією бібліотек та виконувати вправи, щоб отримати більше навичок.

## Приклади додаткових матеріалів

- [Документація BeautifulSoup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Документація requests](https://requests.readthedocs.io/en/latest/)
- [Документація lxml](https://lxml.de/)
- [Приклади коду для парсингу HTML](https://realpython.com/beautiful-soup-web-scraper-python/)