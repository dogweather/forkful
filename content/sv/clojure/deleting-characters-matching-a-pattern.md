---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Clojure: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster är en vanlig uppgift när man arbetar med textbehandling och regelbundna uttryck. Det kan användas för att rensa data, formatera text eller manipulera strängar på olika sätt. Med hjälp av Clojure och dess kraftfulla mönstermatchningsfunktioner kan du enkelt utföra detta och automatisera repetitiva uppgifter.

## Så här gör du

För att ta bort tecken som matchar ett visst mönster finns det flera olika funktioner som kan användas beroende på ditt specifika scenario. Här är några exempel på hur man kan gå tillväga:

- Om du vill ta bort alla förekomster av ett specifikt tecken i en sträng kan du använda funktionen ```clojure (clojure.string/replace "hej hej hej" #"h" "")```, vilket ger dig resultatet "eje eje eje".

- För att ta bort alla icke-numeriska tecken från en sträng kan du använda ```clojure (clojure.string/replace "abc123def456" #"\D" "")```, vilket ger dig resultatet "123456".

- Om du behöver ta bort alla tecken som matchar ett mer komplicerat mönster, till exempel alla ord som börjar på en viss bokstav, kan du använda ```clojure (clojure.string/replace "hello world and hi there" #"\<h[a-z]+\>" "")```, vilket ger dig resultatet "world and there".

Som du kan se använder alla dessa exempel funktionen ```clojure.string/replace```, där det första argumentet är den sträng som ska manipuleras, det andra argumentet är regelbundet uttryck (regex) för det mönster du vill matcha och det tredje argumentet är ersättningstecknet som ska användas.

## Djupdykning

I Clojure används regelbundna uttryck (regex) för att matcha mönster i strängar. I exempel #2 användes uttrycket #"\D" för att matcha alla icke-numeriska tecken. Detta uttryck består av två delar: en backslash (\) som talar om att följande tecken ska tolkas bokstavligt, och D som representerar alla icke-numeriska tecken. Genom att använda dessa två delar tillsammans kan vi enkelt ta bort alla tecken som inte är siffror.

Det finns många fler sätt att använda regex för att matcha mönster och det finns också flera inbyggda funktioner i Clojure som kan användas för att manipulera strängar. Genom att lära dig mer om regex och dessa funktioner kan du upptäcka ännu fler möjligheter för att hantera och manipulera text i Clojure.

## Se även

- [Clojure Docs - String Functions](https://clojure.org/api/cheatsheet)
- [ClojureDocs - Regular Expressions](https://clojuredocs.org/clojure.core/re-matches)
- [Mastering Clojure Strings with Regular Expressions](https://purelyfunctional.tv/guide/mastering-clojure-strings-with-regex/)