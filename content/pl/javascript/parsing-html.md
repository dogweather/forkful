---
title:                "Analiza html"
html_title:           "Javascript: Analiza html"
simple_title:         "Analiza html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Parsing HTML to jest bardzo ważnym aspektem programowania w dzisiejszych czasach. Dzięki niemu możemy manipulować danymi zapisanymi w języku HTML, co daje nam możliwość tworzenia interaktywnych stron internetowych, aplikacji webowych oraz wielu innych przydatnych narzędzi.

## Jak to zrobić

Aby rozpocząć parsowanie HTML w Javascript, musimy najpierw utworzyć nowy obiekt typu `Document`, który reprezentuje załadowany dokument HTML. Możemy to zrobić, korzystając z metody `document.createDocument()`. Następnie, używając odpowiednich metod, możemy odczytać zawartość dokumentu HTML i przetworzyć ją do postaci, którą będzie można wykorzystać w naszym kodzie. Poniżej znajdziesz przykładowy kod wraz z wypisanym rezultatem:

```Javascript
const doc = document.createDocument();
doc.getElementsByTagName("h1")[0].innerText; //Zwraca zawartość pierwszego nagłówka h1 w dokumencie.
```

W powyższym przykładzie używamy metody `getElementsByTagName()` do pobrania wszystkich elementów o tagu `h1` z dokumentu i wyświetlamy ich zawartość za pomocą właściwości `innerText`.

## Głębszy wgląd

Podczas parsowania HTML, istnieje wiele innych przydatnych metod, które można wykorzystać. Należą do nich między innymi `getElementById()`, `getElementsByClassName()`, `querySelector()` i wiele więcej. Warto również wspomnieć o tym, że parsowanie HTML może również być używane do walidacji kodu HTML, dzięki czemu nasze strony internetowe będą działać poprawnie.

## Zobacz również

Poniżej znajduje się lista przydatnych linków, które mogą pomóc Ci w bardziej szczegółowym poznaniu tematu:

- [Dokumentacja Mozilla Developer Network](https://developer.mozilla.org/pl/docs/Web/API/Document)
- [W3Schools Tutorial: Parsowanie HTML przy użyciu Javascript](https://www.w3schools.com/js/js_htmldom_parse.asp)
- [Ebook "Learn to Parse HTML in Javascript" autorstwa Joshue Odmark](https://www.gitbook.com/book/joshdick/learn-to-parse-html-in-javascript/details)