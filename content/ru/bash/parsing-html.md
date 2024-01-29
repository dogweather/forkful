---
title:                "Разбор HTML"
date:                  2024-01-28T23:59:54.967642-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Парсинг HTML означает вникание в структуру и содержимое файла HTML для извлечения информации. Программисты делают это для доступа к данным, манипулирования содержимым или скрапинга веб-сайтов.

## Как это сделать:

Bash не является основным инструментом для парсинга HTML, но это можно сделать с помощью таких инструментов как `grep`, `awk`, `sed`, или внешних утилит, например, `lynx`. Для надежности мы будем использовать `xmllint` из пакета `libxml2`.

```bash
# Установите xmllint при необходимости
sudo apt-get install libxml2-utils

# Пример HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Пример страницы</title>
</head>
<body>
  <h1>Привет, Bash!</h1>
  <p id="myPara">Bash может меня прочитать.</p>
</body>
</html>
EOF

# Парсим заголовок
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Заголовок: $title"

# Извлекаем параграф по ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Содержимое параграфа: $para"
```

Вывод:
```
Заголовок: Пример страницы
Содержимое параграфа: Bash может меня прочитать.
```

## Глубже

В прошлые времена программисты использовали инструменты на основе регулярных выражений, такие как `grep`, для сканирования HTML, но это было неудобно. HTML - это не регулярное выражение, он контекстный. Традиционные инструменты упускают это и могут быть подвержены ошибкам.

Альтернативы? Их предостаточно. Python с Beautiful Soup, PHP с DOMDocument, JavaScript с DOM парсерами — языки с библиотеками, предназначенными для понимания структуры HTML.

Использование `xmllint` в bash-скриптах подходит для простых задач. Он понимает XML, и, соответственно, XHTML. Обычный HTML может быть непредсказуемым, ведь он не всегда следует строгим правилам XML. `xmllint` принуждает HTML следовать модели XML, что хорошо работает для правильно сформированного HTML, но может спотыкаться на сложном содержимом.

## См. также

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): Разъясняет HTML DOM.
- [MDN Web Docs - Парсинг и сериализация XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): О принципах парсинга XML, которые применимы к XHTML.
- [Документация Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Библиотека Python для парсинга HTML.
- [Документация libxml2](http://xmlsoft.org/): Детали о `xmllint` и связанных инструментах XML.
