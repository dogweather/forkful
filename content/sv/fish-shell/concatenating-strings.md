---
title:    "Fish Shell: Sammanfogning av strängar."
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför
Att sätta ihop strängar, eller "concatenate strings" som det heter på engelska, är en vanlig uppgift inom programmering. Genom att lära sig denna teknik kan du manipulera och sammanfoga textsträngar för att skapa mer dynamiska och flexibla program.

## Så här gör du
Det första steget för att sätta ihop strängar i Fish Shell är att använda kommandot `string join`. Detta kommando tar emot två argument, en strängseparator och en lista med strängar som ska sättas ihop.

```Fish Shell
set fruits apples bananas oranges  # Definiera en lista av strängar
string join , $fruits  # Sätter ihop strängarna med kommatecken som separator
```

Resultatet av detta kommer att vara `apples, bananas, oranges`.

## Djupdykning
Det finns flera sätt att manipulera och sätta ihop strängar i Fish Shell. En av dessa är att använda kommandot `string replace`, som låter dig byta ut en del av en sträng med en annan. En annan användbar teknik är att använda variabler, vilket låter dig lagra strängar och sedan sätta ihop dem på olika sätt.

En annan användbar funktion är `string length`, som ger dig längden på en sträng. Detta kan vara användbart om du vill kontrollera att en sträng är inom en viss längd, eller om du vill skapa en loop som går igenom varje tecken i en sträng.

## Se även
- [Fish Shell officiell dokumentation om strängkonkaternering](https://fishshell.com/docs/current/cmds/string.html#concat)
- [En enkel guide för att konkatenera strängar i Fish Shell](https://www.codelle.dev/blog/concatenate-strings-in-fish-shell/)
- [Mer information om hur man manipulerar strängar i Fish Shell](https://www.tecmint.com/string-manipulation-in-fish/)