---
title:                "Разбор HTML"
date:                  2024-01-29T00:00:28.445820-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Парсинг HTML — это процесс извлечения информации из кода HTML, это как находить иголки в стоге сена, если представить, что стог сена состоит из тегов, а иголки — это данные, которые вам нужны. Программисты делают это для того, чтобы извлекать данные с веб-сайтов, что может быть чем угодно, от заголовков на новостном сайте до цен в интернет-магазине.

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
