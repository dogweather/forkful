---
title:                "Utvinna delsträngar"
html_title:           "Clojure: Utvinna delsträngar"
simple_title:         "Utvinna delsträngar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Ibland behöver vi bara en del av en sträng istället för hela strängen. Kanske vill vi få tag på en specifik del av en webbadress eller plocka ut ett namn från en lång lista. Genom att använda Clojure-funktioner för att extrahera substrängar, kan vi snabbt och enkelt få ut den information vi behöver.

## Hur man gör det

För att extrahera substrängar i Clojure använder vi funktionen `subs`. Vi anger startindex för den del av strängen vi vill ha och sedan avslutar vi med ett valfritt slutindex.

```Clojure
;; Definiera en sträng för exempel
(def sträng "Det här är en lång mening")

;; Extrahera "här är"
(subs sträng 4 10) ; "här är"

;; Extrahera "mening"
(subs sträng 15) ; "mening"

```

Vi kan också använda funktionen `clojure.string/substring` för att extrahera substrängar, som har samma syntax som `subs`.

```Clojure
;; Extrahera "en lång"
(clojure.string/substring sträng 11 17) ; "en lång"
```

## Djupdykning

Förutom `subs` och `substring`, finns det andra funktioner som kan användas för att extrahera substrängar i Clojure. Till exempel `reverse` för att vända på strängen och sedan använda `subs` för att få den del som vi vill ha.

```Clojure
;; Extrahera "lång"
(subs (reverse sträng) 0 4) ; "lång"
```

Vi kan också använda reguljära uttryck för att extrahera en del av en sträng. Genom att använda `re-find` och ett reguljärt uttryck som matchar den del vi vill få, kan vi extrahera substrängen.

```Clojure
;; Extrahera texten efter fyra siffror och ett mellanslag
(re-find #"\d{4} " "1987 Clojure Fun") ; "Clojure Fun"
```

## Se även

- [ClojureDocs: String Functions](https://clojuredocs.org/clojure.core#string)
- [Regular Expressions in Clojure](https://clojure.org/reference/regular_expressions)