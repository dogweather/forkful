---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:54.967642-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Bash \u043D\u0435 \u044F\u0432\u043B\u044F\u0435\u0442\u0441\u044F\
  \ \u043E\u0441\u043D\u043E\u0432\u043D\u044B\u043C \u0438\u043D\u0441\u0442\u0440\
  \u0443\u043C\u0435\u043D\u0442\u043E\u043C \u0434\u043B\u044F \u043F\u0430\u0440\
  \u0441\u0438\u043D\u0433\u0430 HTML, \u043D\u043E \u044D\u0442\u043E \u043C\u043E\
  \u0436\u043D\u043E \u0441\u0434\u0435\u043B\u0430\u0442\u044C \u0441 \u043F\u043E\
  \u043C\u043E\u0449\u044C\u044E \u0442\u0430\u043A\u0438\u0445 \u0438\u043D\u0441\
  \u0442\u0440\u0443\u043C\u0435\u043D\u0442\u043E\u0432 \u043A\u0430\u043A `grep`,\
  \ `awk`, `sed`, \u0438\u043B\u0438\u2026"
lastmod: '2024-03-13T22:44:45.366098-06:00'
model: gpt-4-0125-preview
summary: "Bash \u043D\u0435 \u044F\u0432\u043B\u044F\u0435\u0442\u0441\u044F \u043E\
  \u0441\u043D\u043E\u0432\u043D\u044B\u043C \u0438\u043D\u0441\u0442\u0440\u0443\u043C\
  \u0435\u043D\u0442\u043E\u043C \u0434\u043B\u044F \u043F\u0430\u0440\u0441\u0438\
  \u043D\u0433\u0430 HTML, \u043D\u043E \u044D\u0442\u043E \u043C\u043E\u0436\u043D\
  \u043E \u0441\u0434\u0435\u043B\u0430\u0442\u044C \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E \u0442\u0430\u043A\u0438\u0445 \u0438\u043D\u0441\u0442\u0440\
  \u0443\u043C\u0435\u043D\u0442\u043E\u0432 \u043A\u0430\u043A `grep`, `awk`, `sed`,\
  \ \u0438\u043B\u0438 \u0432\u043D\u0435\u0448\u043D\u0438\u0445 \u0443\u0442\u0438\
  \u043B\u0438\u0442, \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440, `lynx`."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

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
