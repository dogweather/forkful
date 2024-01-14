---
title:                "Clojure: Att använda reguljära uttryck"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regex är ett kraftfullt verktyg inom programmering som hjälper dig att söka och manipulera text på ett effektivt sätt. Oavsett om du vill filtrera användardata, validera inmatning eller extrahera information från en stor textmängd, så är regex ett oumbärligt verktyg att ha i din programmeringsverktygslåda. 

## Hur du använder regex i Clojure

```Clojure
;; Importera regex-biblioteket
(require '[re-find :as rf])

;; Hitta alla siffror i en sträng
(rf #"\d+" "I år fyller Clojure 13 år")
;; Output: "13"

;; Hitta alla ord på tre bokstäver
(rf #"\b\w{3}\b" "Fri-yes, ja, si, da!")
;; Output: ("yes" "ja" "si" "da")
```

För att använda regex i Clojure behöver du importera regex-biblioteket, som innehåller funktioner som re-find och re-matches. Dessa funktioner tar emot ett regex-mönster och en sträng som argument och returnerar önskad data.

Regex-mönster börjar alltid med ett #-tecken, följt av ett citattecken och det egentliga mönstret inuti. Regex-mönster kan innehålla metakaraktärer som klassificerar olika typer av tecken, t.ex. \w för alla alfanumeriska tecken och \d för siffror.

## Djupdykning

Regex kan tyckas lite krångligt i början, men med lite övning kommer du snart att få en känsla för det. Här är några tips att tänka på:

- Använd \b för att matcha ordgränser och undvika att få oönskade delsträngar.
- Om du behöver matcha flera alternativ, använd | som står för "eller".
- Om du behöver matcha ett visst antal förekomster av ett tecken eller grupp, använd {n,m} där n är det minsta antalet och m är det högsta antalet förekomster som ska matchas. Till exempel #"\w{3,5}" matchar alla ord på mellan 3 och 5 tecken.

## Se även

- [Clojure regex-biblioteket](https://clojuredocs.org/clojure.core/re-groups)
- [Regexp Cheat Sheet för Clojure](https://gist.github.com/robert-stuttaford/7117281)