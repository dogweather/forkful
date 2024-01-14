---
title:                "Clojure: Söka och ersätta text"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

I dagens digitala värld är det ofta nödvändigt att kunna söka och byta ut text i stora mängder av data. Det kan handla om att korrigera stavfel i en text, ändra formatet på ett dokument eller helt enkelt ersätta ett ord med ett annat. I denna bloggpost kommer vi gå igenom hur du kan lösa detta i Clojure, ett funktionellt programmeringsspråk som blivit alltmer populärt i de senaste åren.

## Hur man gör

För att söka och byta ut text i Clojure kan du använda funktionen "replace" som tar emot tre argument: en sträng som du vill söka igenom, en söksträng och en ersättningssträng. Här är ett enkelt exempel:

```Clojure
(def str "Hej, jag heter Alice")
(replace str "Alice" "Bob")
```

Outputen från detta kommer vara "Hej, jag heter Bob" då vi ersätter söksträngen "Alice" med "Bob". Det är även möjligt att söka och ersätta mer avancerade mönster med hjälp av reguljära uttryck. Till exempel, om vi vill byta ut alla siffror i en sträng med bokstäver, kan vi använda följande kod:

```Clojure
(def str "123 abc 456")
(replace str #"\d" "X")
```

Outputen blir då "XXX abc XXX" där alla siffror har ersatts med "X".

## Djupdykning

För att förstå mer om hur sökning och ersättning fungerar i Clojure kan vi titta på implementeringen av "replace" funktionen. Det finns flera olika sätt att göra det på, men en enkel lösning skulle kunna se ut så här:

```Clojure
(defn replace [str search replace]
  (->> (seq str) ; konverterar strängen till en sekvens av tecken
    (map #(if (= % search) replace %)) ; byter ut tecknen som matchar söksträngen med ersättningstecknet
    (apply str))) ; konverterar sekvensen tillbaka till en sträng
```

I denna kod används funktionerna "seq" för att konvertera strängen till en sekvens av tecken, "map" för att byta ut tecknen och "apply" för att återgå till en sträng igen. Dessa funktioner är grundläggande inom funktionell programmering och finns tillgängliga i Clojure.

## Se även

- Officiell Clojure hemsida: https://clojure.org/
- Reguljära uttryck i Clojure: https://www.braveclojure.com/regular-expressions/
- Bygg- och utvecklingsverktyg för Clojure: https://leiningen.org/