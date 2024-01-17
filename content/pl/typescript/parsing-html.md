---
title:                "Analizowanie html"
html_title:           "TypeScript: Analizowanie html"
simple_title:         "Analizowanie html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Parsowanie HTML to proces analizowania kodu HTML i konwersji go na strukturalny model danych, który można łatwo przetwarzać za pomocą kodu programu. Programiści wykonują parsowanie HTML w celu manipulacji i wykorzystania danych zawartych w kodzie HTML, na przykład do tworzenia interaktywnych stron internetowych.

## Jak to zrobić:

```TypeScript
const parser = new DOMParser(); //Tworzenie nowego parsera
const html = `
  <h1>Hello World!</h1>
  <p>This is a sample HTML code.</p>`;

const doc = parser.parseFromString(html, 'text/html'); //Parsowanie kodu HTML

console.log(doc.body.innerHTML); //Output: <h1>Hello World!</h1><p>This is a sample HTML code.</p>
```

## Głębsze zagłębienie:

Parsowanie HTML jest ważnym procesem w tworzeniu stron internetowych. Po raz pierwszy zostało wprowadzone w Netscape Navigator w 1997 roku i jest obecnie szeroko stosowane w przeglądarkach internetowych i narzędziach do przetwarzania dokumentów HTML. Istnieją również alternatywne metody parsowania, takie jak metoda regex, ale parser DOM jest uważany za bardziej niezawodny i łatwiejszy w użyciu.

## Zobacz też:

- [Introduction to HTML parsing in JavaScript](https://javascript.info/parsing-html)
- [DOMParser in TypeScript](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [HTML parsing with regex and DOM methods comparison](https://medium.com/swlh/html-parsing-with-regexp-vs-dom-60e263a1f78)