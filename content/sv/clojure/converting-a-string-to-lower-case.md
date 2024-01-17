---
title:                "Omvandla en sträng till små bokstäver"
html_title:           "Clojure: Omvandla en sträng till små bokstäver"
simple_title:         "Omvandla en sträng till små bokstäver"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi pratar om att konvertera en sträng till små bokstäver, så menar vi att göra alla bokstäver i strängen till gemena. Detta är ett vanligt arbetssätt för programmerare när de behöver hantera strängar på ett mer enhetligt sätt.

## Så här gör du:
```Clojure
;; Definiera en sträng
(def string "Hej alla ProgProffs!")

;; Använd funktionen lower-case för att konvertera strängen till små bokstäver
(lower-case string)

;; Resultat:
;; "hej alla progproffs!"
```

## Djupdykning:
Att konvertera strängar till små bokstäver har funnits sedan tidigt 1900-tal, då många skrivmaskiner bara hade gemena bokstäver. Alternativ till att använda lower-case funktionen är att iterera över strängen och konvertera varje bokstav separat eller att använda inbyggda Java-metoder. I Clojure implementeras lower-case med hjälp av Java's Character-klass, som har en metod för just detta ändamål.

## Se även:
- Clojure's dokumentation för lower-case funktionen: https://clojuredocs.org/clojure.core/lower-case
- Java's Character-klass: https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html