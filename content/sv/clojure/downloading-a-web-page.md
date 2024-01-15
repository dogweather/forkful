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

## Varför

Det finns många anledningar till varför du skulle vilja ladda ner en webbsida med Clojure. Det kan vara för att använda informationen för analys eller för att bygga en webbskrapa för att samla in data.

## Så här gör du

För att ladda ner en webbsida i Clojure behöver du använda dig av bibliteket "clj-http" som tillhandahåller funktioner för HTTP-anrop. Först installerar du bibliteket genom att lägga till det i din projekts dependecies.

```Clojure
:dependencies [[clj-http "3.11.0"]]
```

Sedan importerar du biblioteket och skapar en funktion som tar en URL som indata och returnerar innehållet på webbsidan som en sträng.

```Clojure
(require '[clj-http.client :as client])

(defn download-webpage [url]
  (client/get url {:as :string}))
```

Slutligen kan du anropa funktionen med en URL för den webbsida du vill ladda ner och skriva ut innehållet.

```Clojure
(let [webpage (download-webpage "https://www.example.com")]
  (println webpage))
```

Detta kommer att skriva ut innehållet på webbsidan i terminalen.

## Djupdykning

Om du vill ha mer kontroll över HTTP-anropet kan du lägga till fler parametrar i funktionen, såsom header och body för att skicka med i förfrågan. Du kan också använda dig av "clj-time" biblioteket för att hantera tidszoner och datumformat. Se gärna dokumentationen för "clj-http" och "clj-time" för mer information och exempel på hur du kan använda dem.

## Se också

* [clj-http dokumentation](https://github.com/dakrone/clj-http/blob/master/README.md)
* [clj-time dokumentation](https://github.com/clj-time/clj-time/blob/master/README.md)