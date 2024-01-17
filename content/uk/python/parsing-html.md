---
title:                "Розбір html"
html_title:           "Python: Розбір html"
simple_title:         "Розбір html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/parsing-html.md"
---

{{< edit_this_page >}}

Що і чому?

Парсування HTML - це процес отримання даних зі сторінок веб-сайтів або інших джерел, що використовують мову розмітки HTML. Це корисно для програмістів, оскільки дозволяє автоматично збирати великі обсяги даних, що може бути надбанням для аналізу чи використання в інших проектах.

Як це зробити:

```python
import requests
from bs4 import BeautifulSoup

URL = "ваш URL-адреса тут"

page = requests.get(URL)
soup = BeautifulSoup(page.content, 'html.parser')

results = soup.find_all('назва тегу', class_='клас тегу')

for result in results:
    print(result.text)
```

Вглиб:

Історичний контекст: Парсування HTML почалося ще з часів появи веб-сторінок і є необхідною частиною веб-скрапінгу та веб-амбілігування.

Альтернативи: Існують інші мови та бібліотеки для парсування HTML, такі як JavaScript і jQuery. Однак, використання Python є більш зручним і простим для програмістів.

Деталі реалізації: Бібліотека BeautifulSoup надає зручний інтерфейс для парсування HTML, шляхом використання методів, таких як `find` і `find_all`, щоб отримати бажані елементи зі сторінки.

Дивись також:

- Офіційна документація BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Як використовувати BeautifulSoup для парсування HTML: https://www.digitalocean.com/community/tutorials/how-to-scrape-web-pages-with-beautiful-soup-and-python-3 
- Використання бібліотеки requests для отримання сторінок: https://www.edureka.co/blog/web-scraping-with-python/