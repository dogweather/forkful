---
title:    "Clojure: Extrahera substrängar"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför
Det kan finnas många anledningar till att vilja extrahera substrängar i Clojure, men en av de vanligaste är för att bearbeta och manipulera textdata. Genom att kunna plocka ut specifika delar av en sträng kan man enkelt utföra olika operationer och data transformationer.

## Hur man gör det
Extrahera en substräng i Clojure är enkelt, tack vare den inbyggda funktionen `subs`, som tar emot en sträng och en start- och slutposition som argument. Här är ett exempel på hur man extraherar en substräng från en sträng:

```Clojure
(def sträng "Hej, det här är en textsträng")
(subs sträng 4 8)
```

Detta skulle returnera "det ", eftersom vi har gett startposition 4 (räknat från 0) och slutposition 8 (den sista positionen är inte inkluderad).

Det är också möjligt att extrahera en delsträng från en viss position till slutet av strängen, genom att bara ange startposition och inte slutposition:

```Clojure
(def sträng "Hej, det här är en textsträng")
(subs sträng 10)
```

Detta skulle returnera "är en textsträng", eftersom vi har gett startposition 10 och inte angett slutposition.

Det kan också vara användbart att extrahera en sträng baserat på ett visst villkor, till exempel om man vill plocka ut alla siffror från en sträng. Detta kan göras med hjälp av `filter`-funktionen och `string/isdigit` från standardbiblioteket `clojure.string`:

```Clojure
(def sträng "abc123")
(->> (seq sträng) (filter string/isdigit) (apply str))
```

Resultatet skulle vara "123", eftersom vi först omvandlar strängen till en sekvens av karaktärer, filtrerar ut allt som inte är en siffra och sätter ihop det till en ny sträng med hjälp av `apply`.

## Djupdykning
För att förstå hur substrängsextraktion fungerar under huven kan det vara bra att veta att strängar i Clojure egentligen är sekvenser av tecken, där varje tecken representeras av sitt numeriska värde. När vi använder `subs`-funktionen, så loopar den igenom alla karaktärer mellan start- och slutpositionen och lägger till dem i en ny sträng som returneras som resultat.

## Se även
- [Clojure.org Docs om Substrings](https://clojuredocs.org/clojure.string/subs)
- [Official Clojure Docs on Substrings](https://clojure.org/reference/strings#substring_expressions)
- [YouTube-tutorial om substrängsextraktion i Clojure](https://www.youtube.com/watch?v=9Ggoa4wsUfg)