---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:28.445820-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u0435\u043C Python, \u0447\u0442\u043E\u0431\u044B \u0432\
  \u044B\u0442\u0430\u0449\u0438\u0442\u044C \u043D\u0435\u043A\u043E\u0442\u043E\u0440\
  \u044B\u0435 \u0434\u0430\u043D\u043D\u044B\u0435 \u0438\u0437 \u043E\u0431\u0440\
  \u0430\u0437\u0446\u0430 HTML \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438 `BeautifulSoup`,\
  \ \u043A\u043E\u0442\u043E\u0440\u0430\u044F \u0434\u0435\u043B\u0430\u0435\u0442\
  \ \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u2026"
lastmod: '2024-03-13T22:44:44.265951-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0435\u043C Python, \u0447\u0442\u043E\u0431\u044B \u0432\u044B\
  \u0442\u0430\u0449\u0438\u0442\u044C \u043D\u0435\u043A\u043E\u0442\u043E\u0440\u044B\
  \u0435 \u0434\u0430\u043D\u043D\u044B\u0435 \u0438\u0437 \u043E\u0431\u0440\u0430\
  \u0437\u0446\u0430 HTML \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438 `BeautifulSoup`, \u043A\u043E\
  \u0442\u043E\u0440\u0430\u044F \u0434\u0435\u043B\u0430\u0435\u0442 \u043F\u0430\
  \u0440\u0441\u0438\u043D\u0433 \u043B\u0435\u0433\u043A\u0438\u043C."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
Давайте используем Python, чтобы вытащить некоторые данные из образца HTML с помощью библиотеки `BeautifulSoup`, которая делает парсинг легким. Если вы еще не установили этот пакет, сделайте это с помощью `pip install beautifulsoup4`.

```Python
from bs4 import BeautifulSoup

# Представим, это ваш HTML
html_doc = """
<html>
<head>
    <title>История Сони</title>
</head>
<body>
    <p class="title">
        <b>История Сони</b>
    </p>
    <p class="story">Жили-были три маленькие сестрички, и звали их
        <a href="http://example.com/elsie" class="sister" id="link1">Эльзи</a>,
        <a href="http://example.com/lacie" class="sister" id="link2">Лейси</a> и
        <a href="http://example.com/tillie" class="sister" id="link3">Тилли</a>;
        и жили они на дне колодца.</p>
</body>
</html>
"""

# Обработаем через Soup
soup = BeautifulSoup(html_doc, 'html.parser')

# Найдем тег title
title_tag = soup.title
print("Название истории:", title_tag.string)

# Найдем все теги 'a' с классом 'sister'
sister_tags = soup.find_all('a', class_='sister')
print("Имена сестер и URL-адреса:")
for sister in sister_tags:
    print(f"- Имя: {sister.string}, URL: {sister['href']}")
```

Вывод будет следующим:

```
Название истории: История Сони
Имена сестер и URL-адреса:
- Имя: Эльзи, URL: http://example.com/elsie
- Имя: Лейси, URL: http://example.com/lacie
- Имя: Тилли, URL: http://example.com/tillie
```

## Более глубокое погружение
В заре интернета HTML анализировали с помощью регулярных выражений и большой доли надежды. Это было беспорядочно, так как HTML не всегда аккуратен и предсказуем. Тогда на сцену вышли библиотеки вроде BeautifulSoup, которые навигируют по древовидной структуре HTML, предлагая нежный способ разрезать и крошить данные.

Существуют также альтернативы, такие как `lxml` и `html.parser`, которые сам BeautifulSoup может использовать в качестве парсеров. `lxml` работает быстрее, но менее толерантен к плохому HTML, в то время как `html.parser` медленнее, но не заботится о сломанных тегах.

Под капотом эти библиотеки строят дерево разбора, превращая теги в объекты, с которыми можно взаимодействовать. BeautifulSoup похож на дружелюбный интерфейс к этим парсерам, переводя ваши вопросы — вроде "Какой заголовок?" или "Есть ли здесь ссылки?" — в действия на дереве.

## См. также
- Документация BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Введение в анализ HTML с помощью регулярных выражений (и почему этого не стоит делать): https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags
- Веб-скрапинг с Python (практическое руководство): https://realpython.com/beautiful-soup-web-scraper-python/
