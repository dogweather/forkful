---
title:    "Fish Shell: Att ta bort tecken som matchar ett mönster"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Vissa gånger när vi arbetar med Fish Shell kanske vi behöver radera vissa tecken som matchar ett specifikt mönster. Detta kan vara till exempel om vi vill ta bort siffror från en sträng eller ta bort alla bindestreck från ett telefonnummer. Att kunna utföra denna typ av uppgift kan hjälpa oss att rensa och hantera data mer effektivt.

## Så här gör du

För att radera tecken som matchar ett visst mönster i Fish Shell kan vi använda kommandot `sed`. Detta kommando låter oss manipulera textfiler, inklusive att radera tecken som matchar ett visst mönster.

För att använda `sed` för att radera tecken kan vi skriva följande kommando i vårt fish shell:

```Fish Shell
sed 's/[mönster]//g' [filnamn]
```

Vi byter ut "[mönster]" med det specifika mönstret vi vill matcha och "[filnamn]" med namnet på den fil vi vill ändra. Till exempel, om vi vill ta bort alla siffror från en fil som heter "data.txt" skulle vårt kommando se ut så här:

```Fish Shell
sed 's/[0-9]//g' data.txt
```

Detta kommer att ta bort alla siffror från filen "data.txt" och skriva ut resultatet i terminalen.

## Djupdykning

För de som är mer intresserade av hur `sed` fungerar för att radera tecken som matchar ett visst mönster, är det bra att veta att "s" står för "substitution" och "g" står för "global". Detta innebär att vi påverkar alla förekomster av det specifika mönstret i filen.

Vi kan också använda olika flaggor för att anpassa vår raderingsprocess. Till exempel, om vi bara vill radera det första tecknet som matchar mönstret, kan vi lägga till flaggan "1" efter mönstret:

```Fish Shell
sed 's/[mönster]//1' [filnamn]
```

Det finns många andra användbara flaggor som vi kan utforska för att hitta det rätta sättet att radera tecken som matchar ett visst mönster för vårt specifika ändamål.

## Se även

- [Fish Shell documentation](https://fishshell.com/docs/3.3/cmds/sed.html)
- [Så här använder du `sed` för att söka och ersätta textfiler i Linux](https://www.howtogeek.com/666395/how-to-use-sed-to-search-and-replace-text-files-in-linux/)