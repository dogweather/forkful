---
title:                "Надсилання http запиту"
html_title:           "Gleam: Надсилання http запиту"
simple_title:         "Надсилання http запиту"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Зачем
Укладення HTTP-запитів є ключовим елементом веб-програмування. Інформація, яку можна отримати через HTTP-запити, допоможе вам створити потужні та ефективні веб-додатки. 

## Як це зробити
```Gleam
let request = http.request("https://example.com")
|> http.get
```
Цей код використовує модуль HTTP, щоб створити HTTP-запит до вказаного URL. Потім результат передається в функцію "get" для виконання запиту та отримання відповіді. 

## Глибше
HTTP-запити є важливою частиною взаємодії з серверами та веб-додатками. Глибше збережена інформація, така як заголовки і тіло відповіді, можуть бути отримані та оброблені за допомогою Gleam. Додаткові можливості, такі як встановлення заголовків запиту та передача даних з тілом запиту, дозволяють гнучко керувати HTTP-запитом.

## Дивись також
- [Документація Gleam з HTTP](https://gleam.run/modules/http.html)
- [Базові принципи HTTP на MDN](https://developer.mozilla.org/uk/docs/Web/HTTP/Overview)
- [Стаття "Зрозуміти HTTP-запити" на Proglib](https://proglib.io/p/pervom-of-vse-ponyat-chto-neobhodimo-dlya-ponimaniya-http-zaprov)