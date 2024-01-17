---
title:                "Ladda ner en webbsida"
html_title:           "Clojure: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta information och data från en specifik webbadress och visa den på en enhet, såsom en dator eller en mobiltelefon. Programerare gör detta för att hämta data från en specifik webbplats och bearbeta den på ett sätt som passar deras behov eller applikation.

## Så här gör du:
Det finns många olika sätt att ladda ner en webbsida på, men i sin enklaste form använder man sig av ett bibliotek som kallas för "clj-http". Här är ett exempel på hur man skulle kunna ladda ner en webbsida med hjälp av detta bibliotek:

```Clojure
(require '[clj-http.client :as http])

(def result (http/get "https://www.example.com"))
(println (:status result))
(println (:body result))
```

Resultatet av detta kodexempel skulle bli status 200 (vilket innebär att sidan har laddats ner utan problem) och innehållet på sidan.

## Djupdykning:
Att ladda ner en webbsida är en viktig del av många webbapplikationer och kan användas för att skapa användare, insamla data eller hämta information från andra webbplatser. I Clojure finns det flera olika bibliotek för att ladda ner webbsidor, såsom HttpKit, clj-webdriver och jsoup. Det är viktigt att välja det bästa biblioteket för ens specifika behov och applikation.

## Se även:
- [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- [https://github.com/http-kit/http-kit](https://github.com/http-kit/http-kit)
- [https://github.com/semperos/clj-webdriver](https://github.com/semperos/clj-webdriver)
- [https://jsoup.org/](https://jsoup.org/)