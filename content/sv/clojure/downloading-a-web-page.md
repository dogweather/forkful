---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:43:48.053314-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta dess innehåll över internet. Programmerare gör detta för att samla data, automatisera tester eller övervaka tillgänglighet.

## How to:
I Clojure kan du använda `clj-http` biblioteket för att enkelt ladda ner innehållet på en webbsida. Här är ett exempel:

```clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (let [response (client/get url)]
    (:body response)))

;; Använd funktionen
(println (download-page "http://example.com"))
```

Kör ovan kod och du skulle se HTML-innehållet i `http://example.com` som output.

## Deep Dive:
Att ladda ner webbsidor är inte nytt. Från kommandoradsverktyg som `wget` till olika bibliotek i många programmeringsspråk, har utvecklare gjort detta sedan internet blev mainstream. Alternativ till `clj-http` i Clojure inkluderar `http-kit` och Java-biblioteket `Jsoup` som också kan hantera parsing.

När du använder `clj-http`, händer kommunikationen över Apache HttpComponents, vilket är en mogen Java-bibliotek för HTTP-kommunikation. Det ger dig en hög kontroll över HTTP-förfrågningar, som att hantera headers, cookies, och timeouts.

## See Also:
- [clj-http GitHub page](https://github.com/dakrone/clj-http)
- [HttpComponents](https://hc.apache.org/)
- [ClojureDocs](https://clojuredocs.org/) - en bra källa för Clojure dokumentation och exempel.
