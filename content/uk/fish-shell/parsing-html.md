---
title:                "Розбір html"
html_title:           "Fish Shell: Розбір html"
simple_title:         "Розбір html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Що і для чого?

Парсинг HTML - це процес отримання даних з веб-сторінок шляхом аналізування їх HTML-коду. Це корисний інструмент для програмістів, оскільки дає можливість отримати потрібні дані з сайтів в автоматизованому режимі.

## Як це зробити:

Користуючись Fish Shell, можна легко парсити HTML-код завдяки вбудованим утилітам, таким як `curl` і `sed`. Нижче наведено приклад коду, який отримує заголовок сторінки за допомогою `curl` і видаляє непотрібні теги за допомогою `sed`.

```
curl -s https://example.com | sed 's/<[^>]\+>/ /g' | awk '/<title>/ {print $2}'
```
Вивід: "Example Domain"

## Глибинне занурення:

Зараз парсинг HTML є широко використовуваним інструментом для скрапінгу даних з інтернету. До появи спеціалізованих бібліотек, програмісти використовували інші інструменти, наприклад, Python із бібліотекою Beautiful Soup. Однак, з введенням Fish Shell і утиліт `curl` і `sed`, парсинг HTML став більш доступним і проще виконується одним рядком коду.

## Дивись також:

* [Офіційна документація Fish Shell](https://fishshell.com/docs/current/)
* [Керівництво з використання `curl` і `sed`](https://www.baeldung.com/curl-sed)
* [Beautiful Soup бібліотека для Python](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)