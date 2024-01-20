---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Ladda ner en webbsida innebär att hämta dess kod och resurser från servern. Programmerare gör det för att analysera innehållet, för att skrapa data, eller för att testa sidans prestanda.

## Hur till:
Här är ett enkelt sätt att ladda ner en webbsida med Clojure:
```clojure
(require '[clj-http.client :as client])

(defn download-web-page [url]
  (let [response (client/get url)]
    (:body response)))
```
För att ladda ner exempelsidan "https://www.example.com", använd bara:
```clojure
(download-web-page "https://www.example.com")
```
Och sidans HTML kommer att visas i din terminal.

## Djupdykning
Historiskt sett har webbskrapning - det vill säga processen att ladda ner och analysera webbsidor - varit en avgörande metod för webbdatainsamling.

Alternativa metoder till att ladda ner en webbsida i Clojure kan innefatta användning av andra bibliotek, till exempel `Jsoup` eller `HtmlUnit`. Dessa erbjuder ytterligare funktionaliteter, som JavaScript-support och förmåga att interagera med sidan.

Om vi dyker djupare in i implementeringen, använder koden ovan clj-http, ett Clojure-bibliotek för att göra HTTP-förfrågningar. `client/get` funktionen skickar en GET-förfrågan till den angivna URL:en och returnerar ett svar som innehåller en body, vilket är HTML-koden av sidan.

## Se också
För mer information och resurser om att ladda ner webbsidor med Clojure, se följande länkar:


- Web scraping med Clojure: [https://kimh.github.io/clojure-by-example/](https://kimh.github.io/clojure-by-example/)

- Jsoup dokumentation: [https://jsoup.org/](https://jsoup.org/) 

- HtmlUnit dokumentation:  [http://htmlunit.sourceforge.net/](http://htmlunit.sourceforge.net/)