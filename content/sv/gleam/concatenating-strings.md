---
title:    "Gleam: Sammanslående av strängar"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

I den här bloggposten kommer vi att utforska hur man enkelt kan kombinera strängar i Gleam-programmeringsspråket. Att kunna kombinera strängar är en viktig del av att skapa dynamiska och interaktiva applikationer, så det är ett viktigt koncept att behärska när man lär sig Gleam.

## Hur man gör

För att sammanfoga två strängar i Gleam använder man sig av operatorn `++` (konkatenering). Låt oss titta på ett enkelt exempel:

```Gleam
fn main() {
    let str1 = "Hej";
    let str2 = "världen!";
    let resultat = str1 ++ str2;
    // result: "Hej världen!"
    io.println(resultat);
}
```

I det här exemplet sätter vi ihop strängarna "Hej" och "världen!" till en ny sträng som vi sedan skriver ut med hjälp av `io.println()`-funktionen.

Det är viktigt att notera att `++`-operatorn endast fungerar för att kombinera strängar. Om man försöker använda den med andra datatyper som till exempel heltal eller booleska värden kommer man att få ett felmeddelande.

## Djupdykning

Det finns lite mer att veta när det gäller att konkatenera strängar i Gleam. Till exempel går det att kombinera fler än två strängar samtidigt genom att enkelt lägga till fler `++`-operatorer mellan dem. Det går också att använda `++` tillsammans med variabler som redan innehåller en sträng, som i följande exempel:

```Gleam
fn main() {
    let str1 = "Gleam är";
    let str2 = "super";
    let str3 = "enkelt!";
    let resultat = str1 ++ " " ++ str2 ++ " " ++ str3;
    // result: "Hej super enkelt!"
    io.println(result);
}
```

Man kan också konkatenera strängar tillsammans med andra datastrukturer, till exempel listor och tupler. Detta gör att man kan bygga upp väldigt flexibla och dynamiska applikationer med hjälp av Gleam.

## Se även

* [Gleams officiella dokumentation för strängmanipulation](https://gleam.run/book/std/string-manipulation.html)
* [En enkel introduktion till Gleam](https://www.cs.usfca.edu/~carnaval/makeLessons/lessons3.html) (på engelska)