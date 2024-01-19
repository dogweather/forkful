---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Nedlasting av en nettside betyr å hente ned data og kode som lager selve nettsiden. Vi programmerere gjør dette for blant annet å analysere innholdet, teste hastigheten, eller hente bestemt informasjon.

## Hvordan:
Clojure bruker en innebygd funksjon `slurp` for å laste ned webinnhold. Så enkelt kan det gjøres:

```Clojure
(defn download-page [url]
  (slurp url))
```

La oss teste den med en side:

```Clojure
(download-page "http://example.com")
```

Output vil være selve HTML-koden av siden.

## Dyp Dykk
Historisk sett, ble nedlasting av nettsider gjort via terminalprogrammer som `wget` eller `curl`. I dag finnes det hundrevis av biblioteker, i alle programmeringsspråk, for denne oppgaven. Clojure sin innebygde `slurp` funksjon er faktisk ganske høy-nivå sammenlignet med mange alternative løsninger: den gjør mye arbeid i bakgrunnen, som å håndtere nettverkstilkoblinger og strømming av data. Hvis du trenger mer kontroll, f. eks. For å sende spesielle HTTP-headers, kan du bruke mer komplekse biblioteker som `clj-http`.

## Se Også
- Clj-http: https://github.com/dakrone/clj-http
- En tutorial på `slurp`: http://clojure-doc.org/articles/tutorials/reading_urls.html
- Wget: https://www.gnu.org/software/wget/
- Curl: https://curl.se/