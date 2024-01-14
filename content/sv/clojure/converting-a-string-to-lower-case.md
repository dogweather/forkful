---
title:    "Clojure: Konvertera en sträng till små bokstäver"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till gemener (lower case) är en vanlig uppgift inom programmering, och kan ofta vara nödvändigt för att uppnå önskat resultat. Genom att lära sig denna teknik, kan du enklare hantera och manipulera strängar i dina Clojure-program.

## Hur
För att konvertera en sträng till gemener i Clojure, kan vi använda funktionen "lower-case". Här är ett exempel på hur man använder denna funktion:

```Clojure
(lower-case "HEJ")
```

Detta kommer att returnera strängen "hej" som output. Om du behöver ändra en hel mening till gemener, kan du använda funktionen "lower-case-first". Här är ett exempel på hur man använder den:

```Clojure
(lower-case-first "VÄLKOMMEN till Sverige!")
```

Detta kommer att returnera strängen "vÄLKOMMEN till sverige!". Som du kan se, konverterar funktionen bara den första bokstaven till gemener.

## Djupdykning
När vi använder funktionen "lower-case" eller "lower-case-first", är det viktigt att tänka på att Unicode i Clojure hanterar diakritiska tecken på ett annat sätt. Till exempel, om vi konverterar strängen "Åsa" till gemener, kommer den att returnera "åSA" eftersom bokstaven "Å" har en annan position i Unicode än bokstaven "A". För att undvika detta kan vi använda funktionen "string/upper-case" innan vi konverterar till gemener, för att först se till att alla diakritiska tecken är på rätt plats.

## Se även
- [Clojure dokumentation för lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [Fler funktioner för strängmanipulering i Clojure](https://clojuredocs.org/clojure.string)
- [Kurs i Clojure-programmering](https://www.coursera.org/learn/clojure) (på svenska)