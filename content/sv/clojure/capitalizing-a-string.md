---
title:    "Clojure: Omvandla en sträng till versaler"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför
Att skriva och läsa kod kan vara både spännande och utmanande. Ofta behöver vi manipulera data för att göra den läsbar och hanterbar för vårt program. En vanlig åtgärd som vi kan behöva utföra är att göra en sträng (eller en sekvens av tecken) börjar med en versal bokstav. Vi ska tala om varför det är användbart och hur man kan göra det i Clojure.

## Hur man gör
Först och främst kan vi använda funktionen `capitalize` för att konvertera en sträng till med inledande versal bokstav till en ny sträng. Se exemplet nedan:

```Clojure
(def str "clojure är ett spännande programmeringsspråk")
(capitalize str)
;=> "Clojure är ett spännande programmeringsspråk"
```

En annan funktion som kan vara användbar är `clojure.string/capitalize` som tillhandahålls av standardbiblioteket `clojure.string`. Den här funktionen tar emot en optional parameter `locale` för att hantera specifika bokstäver som finns i andra språk. Se nedan exempel för hur man skulle kunna använda den:

```Clojure
(require '[clojure.string :as str])

(def str "efterrätt")
(str/capitalize str :swedish)
;=> "Efterrätt"
```

## Djupdykning
Bakom kulisserna använder båda dessa funktioner Clojure's `java.text.BreakIterator` för att identifiera det första icke-bokstavstecknet i strängen och sedan returnerar en ny sträng med den bokstaven kapitaliserad. Det är en väldigt enkel men viktig funktion som hjälper oss att hantera textdata i våra program.

## Se även
- [Officiell Clojure dokumentation för `capitalize`](https://clojure.org/api/clojure.core/capitalize)
- [Officiell Clojure dokumentation för `clojure.string`](https://clojure.github.io/clojure/clojure.string-api.html)
- [Clojure Cheatsheet](https://clojure.org/api/cheatsheet)