---
title:                "Clojure: Utvinna substrängar"
simple_title:         "Utvinna substrängar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Varför extrahera substrängar?

När du arbetar med texter i Clojure kan du ibland behöva extrahera delar av en sträng, även kallad en substräng. Det kan vara för att söka igenom en sträng efter ett visst mönster eller för att manipulera den på ett specifikt sätt. I den här bloggposten kommer vi att gå igenom hur man extraherar substrängar i Clojure och kommer att ge några djupare kunskaper om ämnet.

## Hur man extraherar substrängar

I Clojure finns det flera sätt att extrahera substrängar. Ett vanligt sätt är att använda funktionen `subs`, som tar emot en sträng, en startposition och en slutposition som argument:

```Clojure
(subs "Hej världen" 4 8)
```
Output: `värld`

Detta kodexempel tar en sträng och extraherar delen från position 4 till position 8, vilket i det här fallet är ordet "värld".

En annan funktion för substrängs-extrahering är `substring`, som tar emot samma argument som `subs` men tar också emot ett ytterligare argument för att ange ett steg med vilket man kan hoppa över tecken:

```Clojure
(substring "Hej världen" 0 8 2)
```
Output: `Hjävl`

I det här fallet hoppas vi över varannan bokstav från startpositionen till slutpositionen.

Du kan också använda `re-find` för att söka efter ett visst mönster i en sträng och extrahera substrängen som matchar det mönstret:

```Clojure
(re-find #"l\d" "Lektion 9")
```
Output: `l9`

I det här exemplet söker vi efter ett litet "l" följt av en siffra i strängen "Lektion 9" och `re-find` returnerar substrängen som matchar det mönstret.

## Djupdykning i substrängsextrahering

En viktig sak att tänka på när du extraherar substrängar är indexeringen. I Clojure (precis som i många andra programmeringsspråk) börjar indexeringen av en sträng på position 0, vilket betyder att det första tecknet i en sträng har index 0 och det sista tecknet har index "längden på strängen - 1".

Du kan också använda funktionen `get` för att extrahera ett enskilt tecken från en sträng:

```Clojure
(get "Hej världen" 1)
```
Output: `e`

Även om det finns flera olika sätt att extrahera substrängar i Clojure, är det viktigt att välja rätt funktion baserat på det specifika behovet och argumenten som behövs.

## Se även

- [Dokumentation för subs](https://clojuredocs.org/clojure.core/subs)
- [Dokumentation för substring](https://clojuredocs.org/clojure.core/substring)
- [Dokumentation för re-find](https://clojuredocs.org/clojure.core/re-find)