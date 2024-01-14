---
title:    "Clojure: Att hitta längden på en sträng."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Varför

Att hitta längden på en sträng är ett grundläggande koncept inom programmering, oavsett vilket språk man använder. Det är en viktig färdighet att behärska eftersom strängar är en central del av datahanteringen inom de flesta program.

# Hur man gör

För att hitta längden på en sträng i Clojure, kan man använda funktionen "count". Här är ett enkelt exempel:

```Clojure
(count "Hej!")
```

Detta kommer att returnera värdet 4, eftersom strängen "Hej!" innehåller fyra tecken. Här är ett annat exempel där vi använder "count" för att räkna antalet tecken i en variabel "namn":

```Clojure
(def namn "Jenny")
(count namn)
```

Detta kommer att returnera värdet 5, eftersom "Jenny" innehåller fem bokstäver.

Det är också viktigt att notera att "count" också fungerar på andra typer av datastrukturer, som vektorer och listor. Här är ett exempel där vi använder "count" för att räkna antalet element i en vektor:

```Clojure
(def vektor [1 2 3 4])
(count vektor)
```

Detta kommer att returnera värdet 4, eftersom vektorn innehåller fyra element.

# Fördjupning

För de som är intresserade av att förstå hur funktionen "count" fungerar bakom kulisserna, så är den implementerad som en "seq"-funktion som först omvandlar datan till en sekvens, och sedan räknar antalet element i sekvensen. Detta innebär även att "count" fungerar för alla datatyper som har en sekvens-implementation, inte bara för strängar, vektorer och listor.

# Se även

- Funktionen "count" i Clojure dokumentationen: https://clojuredocs.org/clojure.core/count
- En jämförelse av hur "count" fungerar i olika programmeringsspråk: https://en.wikibooks.org/wiki/Clojure_Programming/Examples/API_Examples/Counting_elements_in_a_sequence