---
title:                "Пошук html"
html_title:           "Fish Shell: Пошук html"
simple_title:         "Пошук html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Для чого нам знадобиться аналізувати HTML-код? Один із підходів можливих – витягнути корисну інформацію для подальшого використання, такого як парсинг веб-сторінок для створення аналітики або збирання даних для автоматичної обробки.

## Як користуватися Fish Shell для парсингу HTML

Для початку, нам знадобиться встановлення Fish Shell – це швидкий і легкий shell, який надає зручний інтерфейс для взаємодії з файловою системою та запуску програм. Далі нам знадобиться встановити імовірно найкращу програму для парсингу HTML – `pup`, яка дозволить нам зручно витягнути необхідні дані з веб-сторінок.

```Fish Shell
apt-get update
apt-get install fish
curl https://github.com/ericchiang/pup/releases/download/v0.4.0/pup_v0.4.0_linux_amd64.zip
unzip pup_v0.4.0_linux_amd64.zip
./pup --color file.html 'span.classname' text
```

Цей приклад дозволяє нам витягнути текст, який міститься в мітці `<span>` з класом "classname" з файла `file.html`. Документація для цього інструменту також доступна на GitHub [тут](https://github.com/ericchiang/pup).

## Глибока занурення

Якщо ви хочете покопатися глибше і розібратися в самому процесі парсингу HTML, ви можете вивчити більше про синтаксис селекторів CSS і XPath, які дозволять вам точніше вибирати елементи зі сторінки.

Ось кілька корисних посилань для подальшого читання:

- [Основи CSS-селекторів](https://www.w3schools.com/cssref/css_selectors.asp)
- [Основи XPath](https://www.w3schools.com/xml/xml_xpath.asp)
- [Документація для розробників Chrome](https://developers.google.com/web/tools/chrome-devtools/css)
- [Стаття на Medium про парсинг HTML з Fish Shell](https://medium.com/crap-a-day/how-to-parse-html-with-fish-shell-5a4d4ec8580f)

## Дивіться також

- [Fish Shell документація](https://fishshell.com/docs/current/index.html)
- [Репозиторій проекту Fish на GitHub](https://github.com/fish-shell/fish-shell)