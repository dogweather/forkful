---
title:                "Söka och ersätta text"
html_title:           "Clojure: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att söka och ersätta text är en vanlig uppgift för programmerare. Det innebär att hitta en specifik del av en text och ersätta den med en annan. Det kan vara användbart för att göra kodändringar, fixa fel eller uppdatera data.

## Hur:

### Enkelt exempel:
Om vi har en sträng "Hej världen!" och vill ersätta "världen" med "clojure", kan vi använda funktionen ```Clojure (replace "världen" "clojure" "Hej världen!")```. Detta kommer att resultera i "Hej clojure!" som output.

### Komplexare exempel:
I följande kod så söker vi efter alla förekomster av "hund" i en lista med djurnamn och ersätter dem med "katt". Sedan använder vi funktionen ```Clojure (into [] (map #(if (= % "hund") "katt" %) ["hund" "katt" "apa" "hund"]))```, vilket resulterar i ```["katt" "katt" "apa" "katt"]``` som output.

## Deep dive:

### Historisk kontext:
Att söka och ersätta text är en vanlig uppgift inte bara för programmerare, utan även för användare av textredigerare eller ordbehandlingssprogram. Det har funnits sedan långt innan programmeringsspråket Clojure, och används idag i många olika sammanhang.

### Alternativ:
Det finns flera olika funktioner i Clojure som kan användas för att söka och ersätta text, såsom ```replace```, ```clojure.string/replace``` och ```clojure.walk/postwalk```. Det är också möjligt att använda reguljära uttryck för att göra avancerade sökningar och ersättningar.

### Implementation:
Funktionen ```replace``` i Clojure använder rekursion för att gå igenom en text och hitta alla förekomster av en viss sträng. När en matchning hittas ersätts den med den nya strängen och funktionen fortsätter sedan tills hela texten har gåtts igenom.

## See also:

För mer information och exempel på hur man kan söka och ersätta text i Clojure, rekommenderas följande länkar:

- Clojure-dokumentation för ```replace```: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/replace
- Officiell guide för reguljära uttryck i Clojure: https://clojuredocs.org/clojure.core/re-matches
- StackOverflow-fråga om användning av reguljära uttryck i Clojure: https://stackoverflow.com/questions/19761968/using-regular-expressions-in-clojure