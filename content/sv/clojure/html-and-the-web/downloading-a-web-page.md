---
date: 2024-01-20 17:43:48.053314-07:00
description: "How to: I Clojure kan du anv\xE4nda `clj-http` biblioteket f\xF6r att\
  \ enkelt ladda ner inneh\xE5llet p\xE5 en webbsida. H\xE4r \xE4r ett exempel."
lastmod: '2024-03-13T22:44:37.524235-06:00'
model: gpt-4-1106-preview
summary: "I Clojure kan du anv\xE4nda `clj-http` biblioteket f\xF6r att enkelt ladda\
  \ ner inneh\xE5llet p\xE5 en webbsida."
title: "H\xE4mta en webbsida"
weight: 42
---

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
