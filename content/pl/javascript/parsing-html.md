---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie HTML to proces analizowania kodu HTML i przekształcania go w użyteczne dane lub strukturę. Programiści robią to, aby łatwiej manipulować strukturą strony, ekstrahować informacje lub inaczej interaktywować z dokumentem HTML.

## Jak to zrobić:
Aby przeprowadzić parsowanie HTML w JavaScript, możemy korzystać z interfejsu DOMParser API. Oto prosta demonstracja:

```Javascript
let tekstHtml = "<h1>Tytuł</h1><p>Paragraf</p>";
let parser = new DOMParser();
let dokument = parser.parseFromString(tekstHtml, "text/html");

console.log(dokument.body.firstChild.textContent);
console.log(dokument.body.lastChild.textContent);
```

Output:

```Javascript
"Tytuł"
"Paragraf"
```

## Na głęboką wodę
Historia parseing HTML jest blisko związana z historią rozwoju języków znaczników. Początkowo parsowanie HTML było skomplikowane i niewielu programistów rozumiało jak to zrobić, ale z biegiem lat stało się to łatwiejsze dzięki narzędziom takim jak jQuery.

Alternatywą dla DOMParser jest innerHTML, ale jest mniej bezpieczny i może prowadzić do ataków typu cross-site scripting (XSS). Inne narzędzia do parsowania, takie jak `htmlparser2` lub `jsdom`, oferują swoje własne funkcje i mogą być lepiej przystosowane do konkretnych zastosowań.

DOMParser jest wbudowany w większość przeglądarek i przekształca ciąg znaków HTML w obiekty DOM, które są łatwe do manipulowania za pomocą JavaScript. Implementacja będzie zależała od szczegółów konkretnej aplikacji i wymagań.

## Zobacz także
- Dokumentacja DOMParser API - [link](https://developer.mozilla.org/pl/docs/Web/API/DOMParser)
- Jak zabezpieczyć swoją stronę przed XSS - [link](https://www.owasp.org/index.php/XSS_(Cross_Site_Scripting)_Prevention_Cheat_Sheet)
- Pakiet npm htmlparser2 - [link](https://npmjs.com/package/htmlparser2)
- Pakiet npm jsdom - [link](https://npmjs.com/package/jsdom)