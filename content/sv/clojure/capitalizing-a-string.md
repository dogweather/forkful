---
title:                "Att sätta stor bokstav på en sträng"
html_title:           "Clojure: Att sätta stor bokstav på en sträng"
simple_title:         "Att sätta stor bokstav på en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle någon vilja ändra storlek på en sträng i Clojure? I många situationer är det viktigt att ha en korrekt formaterad och typsatt sträng, särskilt när man arbetar med data som ska skrivas ut eller jämföras.

## Så här gör man
För att ändra storlek på en sträng i Clojure, använd funktionen `clojure.string/capitalize`. Detta tar en sträng som argument och returnerar strängen med den första bokstaven i varje ord i versal (stor bokstav).

```Clojure
(clojure.string/capitalize "hej där!") ; "Hej Där!"
(clojure.string/capitalize "JAG ÄR EN RUBRIK") ; "Jag Är En Rubrik"
```

Om du vill bara ändra storlek på första bokstaven i en sträng, använd `clojure.string/capitalize-first`.

```Clojure
(clojure.string/capitalize-first "jag är en rubrik.") ; "Jag är en rubrik."
(clojure.string/capitalize-first "hur MÅR du?") ; "Hur Mår Du?"
```

## Djupdykning
Förutom att bara ändra storlek på en hel sträng eller första bokstaven, finns det flera andra funktioner i Clojure för att manipulera en strängs storlek. 
`clojure.string/lower-case` och `clojure.string/upper-case` ändrar alla bokstäver i en sträng till små eller stora bokstäver, medan `clojure.string/capitalize-words` ändrar storleken på alla ord i en sträng till versaler.

```Clojure
(clojure.string/lower-case "HÄLSA PÅ Johan") ; "hälsa på johan"
(clojure.string/upper-case "önska mig LYCKA TILL") ; "ÖNSKA MIG LYCKA TILL"
(clojure.string/capitalize-words "hej där!") ; "Hej Där!"
```

Det finns också funktioner för att ändra storlek på strängen baserat på dess Unicode-kategori, som `clojure.string/capitalize-lower-case` som endast ändrar storlek på ord och siffror och lämnar specialtecken oförändrade.

## Se även
- [Clojure dokumentation för strängmanipulering](https://clojure.org/guides/learn/strings)
- [En handledning för att lära sig Clojure](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
- [Hjälp med Clojure på svenska](https://javacoders.se/group/clojure)