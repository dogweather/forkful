---
title:    "Clojure: Läsning av en textfil"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil kan vara en användbar kunskap inom Clojure-programmering. Det låter dig läsa och manipulera data från en textfil på ett enkelt sätt, vilket kan vara användbart för många olika användningsområden.

## Hur man gör

För att läsa en textfil i Clojure, behöver du först öppna filen med hjälp av inbyggda funktionen "clojure.java.io/resource". Sedan kan du läsa filen rad för rad med hjälp av "clojure.java.io/reader" funktionen. Nedan finns ett kodexempel:

```Clojure
(def file-resource (clojure.java.io/resource "names.txt"))
(def file-reader (clojure.java.io/reader file-resource))

; Läser rad för rad tills filen är slut
(while-some? line (line-seq file-reader)
  (println line))
```

Om vår "names.txt" fil hade följande innehåll:

```
Anna
Karl
Maria
```

Så skulle ovanstående kod skriva ut:

```
Anna
Karl
Maria
```

## Djupdykning

När du läser en textfil i Clojure, är det viktigt att förstå hur filen kommer att läsas. Vanligtvis läses filen rad för rad, så om du vill läsa hela filen samtidigt, måste du använda en loop eller en rekursiv funktion. Det är också viktigt att känna till vad varje rad i filen innehåller, eftersom du behöver ange rätt datastruktur för att lagra den informationen korrekt.

En annan viktig aspekt att tänka på är filens encoding, eller hur tecken kodas. Om du läser en fil med ett annat encoding än standard, måste du specificera det i din kod, annars kan tecken bli otydliga eller felaktiga.

## Se även

- Clojure.io: https://clojure.org/reference/io
- Filmanipulationsfunktioner i Clojure: https://martintrojer.github.io/clojure/2016/06/07/clojure-and-file-io
- Kom i gång med Clojure: http://clojurekoans.com