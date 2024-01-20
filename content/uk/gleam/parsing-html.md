---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Що та навіщо?
Парсинг HTML – це процес, коли ваш код аналізує HTML файл і розбиває його на частини для маніпулювання чи отримання даних. Це головний інструмент для веб скрапінгу або збору даних з веб-сайтів.

## Як це зробити:
```Gleam
fn main() {
   let html = "<html><body><h1>Привіт, світе!</h1></body></html>";
   let document = Html::parse_document(&html);

   let h1_tag = document.select("h1").unwrap().next().unwrap();
   assert_eq!(h1_tag.text(), "Привіт, світе!");
}
```
Вищенаведений код аналізує HTML-рядок, а потім шукає тег "h1" і отримує його текстове значення.

## Поглиблений огляд
Парсинг HTML — досить стара концепція, що з'явилася з появою HTML і необхідністю аналізувати веб-сторінки. Є різні способи робити це, включаючи регулярні вирази або DOM-дерево. Але Gleam використовує бібліотеку для парсингу HTML, що дозволяє працювати з кодом HTML як з об'єктом.

## Дивіться також
[Gleam HTML parsing library](https://github.com/gleam-lang/html)
[Web scraping guide](https://www.dataquest.io/blog/web-scraping-beautifulsoup/)
[Alternative parsing techniques](https://tomassetti.me/parsing-in-python-all-the-tools-and-libraries-you-can-use/)