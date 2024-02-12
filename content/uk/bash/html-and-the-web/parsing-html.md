---
title:                "Аналіз HTML"
aliases: - /uk/bash/parsing-html.md
date:                  2024-02-03T19:11:50.238994-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Парсинг HTML означає проходження через структуру та вміст HTML-файлу для витягування інформації. Програмісти роблять це, щоб отримати доступ до даних, маніпулювати вмістом або скрейпити веб-сайти.

## Як робити:

Bash не є основним інструментом для парсингу HTML, але це можливо засобами на кшталт `grep`, `awk`, `sed` або зовнішніми утилітами на кшталт `lynx`. Для надійності ми використаємо `xmllint` з пакунка `libxml2`.

```bash
# Встановіть xmllint за потреби
sudo apt-get install libxml2-utils

# Зразок HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Sample Page</title>
</head>
<body>
  <h1>Привіт, Bash!</h1>
  <p id="myPara">Bash може мене читати.</p>
</body>
</html>
EOF

# Парсити заголовок
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Заголовок: $title"

# Витягнути параграф за ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Вміст параграфа: $para"
```

Вивід:
```
Заголовок: Sample Page
Вміст параграфа: Bash може мене читати.
```

## Поглиблений огляд

Раніше програмісти використовували інструменти на базі regex, такі як `grep`, для сканування HTML, але це було громіздко. HTML не є регулярним - він контекстуальний. Традиційні інструменти промахуються через це та можуть бути схильні до помилок.

Альтернативи? їх багато. Python із Beautiful Soup, PHP із DOMDocument, JavaScript із DOM-парсерами — мови з бібліотеками, розробленими для розуміння структури HTML.

Використання `xmllint` у bash-скриптах є міцним для простих завдань. Він розуміє XML і, відповідно, XHTML. Звичайний HTML може бути непередбачуваним, однак. Він не завжди слідує строгим правилам XML. `xmllint` примушує HTML слідувати моделі XML, що добре працює для правильно сформованого HTML, але може спотикатись на погано організованому.

## Див. також

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): роз'яснює HTML DOM.
- [MDN Web Docs - Парсинг та серіалізація XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): Для принципів парсингу XML, які застосовуються до XHTML.
- [Документація Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Бібліотека Python для парсингу HTML.
- [Документація libxml2](http://xmlsoft.org/): Деталі про `xmllint` та пов'язані інструменти XML.
