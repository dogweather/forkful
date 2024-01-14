---
title:                "Clojure: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Varför skulle någon vilja kombinera strängar när de programmerar? Kort sagt, det är ett sätt att slå samman flera strängar för att skapa en enda sträng. Detta kan vara användbart när man bygger textutskrifter eller skapar dynamiska meddelanden.

## Hur

För att kombinera strängar i Clojure kan du använda funktionen `str`. Här är ett enkelt exempel:

```Clojure
(str "Hej" " " "världen!")
```

Detta skulle ge följande output:

```
Hej världen!
```

Du kan också använda `str` för att kombinera en sträng med en variabel. Till exempel:

```Clojure
(def s "Jag gillar att programmera i Clojure")
(str s " för det är ett elegant språk.")
```

Detta skulle ge följande output:

```
Jag gillar att programmera i Clojure för det är ett elegant språk.
```

Du kan också använda `str` för att skapa dynamiska meddelanden med hjälp av strängformatering. Till exempel:

```Clojure
(str "Hej " "du är " (str (+ 10 5) " år gammal."))
```

Detta skulle ge följande output:

```
Hej du är 15 år gammal. 
```

## Djupdykning

Bakom kulisserna använder Clojure `StringBuilder` för att kombinera strängar. Detta betyder att det är en effektiv metod för att kombinera strängar, speciellt när det finns en stor mängd strängar som behöver sättas ihop.

Det är också värt att notera att funktionen `str` kan ta ett obegränsat antal argument. Du kan passera in så många strängar som du vill och de kommer att sättas ihop i den ordningen de kommer i. Det kan vara bra att se till att inte glömma bort argumenten om du använder variabler för att sammanslagna strängar.

## Se också

* [Dokumentation för funktionen `str`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/str)
* [En djupare förståelse av strängformatering i Clojure](https://www.braveclojure.com/core-functions-in-depth/#String_Functions)