---
title:                "Skriva till standardfel"
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standardfel är hur program rapporterar fel och diagnostisk info. Programmerare gör detta för att skilja vanlig output från felmeddelanden, vilket gör debugging lättare.

## Hur man gör:
```Clojure
; Skriv ett enkelt felmeddelande till standard error
(.println System/err "Det där gick inte som planerat.")

; Fånga och logga ett undantagsfel till standard error
(try
  (throw (Exception. "Ett oväntat fel inträffade"))
  (catch Exception e
    (.println System/err (.getMessage e))))
```
Sample Output:
```
Det där gick inte som planerat.
Ett oväntat fel inträffade
```

## Djupdykning
Historiskt sett är standardfel en del av Unix konventionen, avsedd för att skilja normal data från felmeddelanden. Alternativ till skrivning till standardfel inkluderar loggbibliotek som `clojure.tools.logging`. Intern implementation använder Java's `System/err` som är bunden till standarfelströmmen.

## Se Också
- Clojure's officiella dokumentation: https://clojure.org/
- `clojure.tools.logging`: https://github.com/clojure/tools.logging
- Java's `System/err`: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err