---
title:                "Sammanfogning av strängar"
html_title:           "Clojure: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar är en vanlig uppgift inom programmering, oavsett vilket språk man använder. Att kombinera flera strängar till en enda kan vara användbart för att bygga texter eller strängar som behövs för vissa operationer. Det kan också vara användbart för att skapa dynamiska meddelanden eller utskrifter baserat på användarinput.

## Hur man gör

Att sammanfoga strängar i Clojure är enkelt och kan göras på flera sätt. Ett sätt är att använda funktionen `str`, som tar emot flera argument och returnerar dem som en sträng. Här är ett exempel på hur detta kan se ut:

```Clojure
(str "Hej" "på" "dig") ; resultat: "Hej på dig"
```

Man kan också använda operatorn `+` för att sammanfoga strängar, vilket gör samma sak som `str`-funktionen. Här är ett exempel på detta:

```Clojure
(+ "Välkommen" "till" "första" "artikeln") ; resultat: "Välkommen till första artikeln"
```

Om man vill sammanfoga en vektor med strängar kan man använda funktionen `reduce` tillsammans med `str`-funktionen. Här är ett exempel:

```Clojure
(reduce str ["Hello" " " "world"]) ; resultat: "Hello world"
```

## Djupdykning

När man sammanfogar strängar i Clojure är det viktigt att veta att strängar är oföränderliga (immutable). Det betyder att när man sammanfogar två strängar, skapas en helt ny sträng istället för att ändra på den befintliga strängen. Detta kan vara värt att hålla i åtanke om man jobbar med stora datamängder och prestanda är viktigt.

Det finns också andra funktioner som kan vara användbara när man sammanfogar strängar, såsom `format`, `clojure.string/join` och `clojure.string/replace`. Det kan vara värt att utforska dessa funktioner för att hitta det bästa sättet att sammanfoga strängar i en specifik situation.

## Se även

- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)
- [The Joy of Clojure](https://www.amazon.com/Joy-Clojure-Thinking-Way/dp/1617291412)
- [Clojure Cookbook](https://clojure-cookbook.com/)