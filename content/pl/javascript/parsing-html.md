---
title:                "Parsowanie HTML"
html_title:           "Javascript: Parsowanie HTML"
simple_title:         "Parsowanie HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?

W programowaniu, parsowanie HTML to proces analizowania struktury kodu HTML, aby móc wyciągnąć potrzebne informacje. Programiści często używają tego narzędzia, aby odczytać treści ze stron internetowych lub aplikacji.

## Jak to zrobić?

```javascript
const htmlContent = "<p>Witaj na mojej stronie!</p>";
const parser = new DOMParser();
const parsedHtml = parser.parseFromString(htmlContent, "text/html");
console.log(parsedHtml.body.textContent);
// Output: Witaj na mojej stronie!
```

## W ciąg głębszy

W przeszłości, parsowanie HTML było trudne, ponieważ nie istniały narzędzia do łatwego parsowania dokumentów HTML. Obecnie, istnieje wiele bibliotek i narzędzi, które ułatwiają ten proces, takie jak jQuery czy Cheerio. Można również użyć języków takich jak Python czy PHP, które również mają wbudowane funkcje do parsowania HTML.

## Zobacz także

- [W3Schools HTML DOM Parser](https://www.w3schools.com/js/js_htmldom.asp)
- [jQuery Official Website](https://jquery.com/)
- [Cheerio on npm](https://www.npmjs.com/package/cheerio)