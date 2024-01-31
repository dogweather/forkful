---
title:                "Разбор HTML"
date:                  2024-01-28T23:59:52.374282-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Парсинг HTML — это процесс декодирования универсального языка интернета для отбора данных или манипулирования содержимым. Программисты парсят HTML, чтобы автоматизировать веб-скрапинг, интегрировать API или конвертировать форматы данных.

## Как это сделать:
Fish Shell не является главным инструментом для парсинга HTML, но с правильными инструментами это выполнимо. Давайте подключим `pup`, командный парсер HTML, для работы с HTML-содержимым.

```fish
# Сначала установим pup
brew install pup

# Извлечем заголовок с example.com
curl -s http://example.com | pup 'title text{}'

# Вывод должен быть названием сайта, например:
# Example Domain
```

Теперь давайте заберем все гиперссылки:

```fish
# Извлечем ссылки (атрибуты href) с example.com
curl -s http://example.com | pup 'a attr{href}'

# Пример вывода:
# http://www.iana.org/domains/example
```

## Погружение
До появления Fish Shell и `pup` люди использовали неуклюжие регулярные выражения или сложные серверные скрипты. Инструменты типа `pup` усовершенствовали процесс, опираясь на синтаксис CSS-селекторов для более интуитивного и надежного парсинга.

Альтернативы включают Beautiful Soup на Python или Cheerio с Node.js; они более мощные, но не такие лаконичные для однострочников.

Парсинг HTML с помощью Fish сводится к передаче задачи специализированным инструментам из-за его ограниченных возможностей манипулирования текстом. Fish вызывает эти инструменты, захватывает их вывод и позволяет вам творить свои скриптовые чудеса.

## Смотрите также
- [Pup GitHub репозиторий](https://github.com/ericchiang/pup) - Документация и примеры.
- [Документация Fish Shell](https://fishshell.com/docs/current/index.html) - Узнайте больше о Fish.
- [Документация Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) - Для более сложного парсинга HTML на Python.
- [Cheerio GitHub репозиторий](https://github.com/cheeriojs/cheerio) - Для тех, кто интересуется подходом на базе JavaScript.
