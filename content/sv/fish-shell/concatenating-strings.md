---
title:    "Fish Shell: Sammanslagning av strängar"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar är en vanlig uppgift när man programmerar. Genom att lära sig hur man konkatenerar strängar kan du effektivt manipulera text och skapa mer dynamiska program.

## Hur man gör

För att sammanfoga strängar i Fish Shell, använder man en inbyggd funktion som heter `string join`. Funktionen tar in en lista av strängar och sätter ihop dem enligt ett valt separator-tecken. Här är ett exempel på hur man kan konkatenera två strängar med ett mellanslag som separator:

```Fish Shell
set str1 "Hej"
set str2 "världen"
echo (string join " " $str1 $str2)
```

Detta kommer att ge följande utmatning:

`Hej världen`

Som du kan se i kodexemplet använder vi `set` för att skapa två variabler, en för varje sträng som vi vill konkatenera. Sedan använder vi `echo` för att skriva ut den sammansatta strängen genom att använda `string join` och ange mellanslag som separator.

## Djupdykning

Det finns flera olika sätt att manipulera och sammansätta strängar i Fish Shell. En annan användbar funktion är `string split` som gör det möjligt att dela upp en sträng baserat på ett valt separator-tecken. Här är ett exempel på hur man kan använda `string split` för att dela upp en sträng i en lista av ord:

```Fish Shell
set str "det här är en mening"
set words (string split " " $str)
echo $words
```

Detta kommer att ge följande utmatning:

`det här är en mening`

Som du kan se i exempelkoden delar vi upp strängen `str` baserat på mellanslag och sätter ihop resultaten i en lista `words`. Sedan använder vi `echo` för att skriva ut listan.

## Se även

- [Fish Shell - dokumentation för strängar](https://fishshell.com/docs/3.1/cmds/set.html#set-string-join)
- [Fish Shell - lista av inbyggda funktioner](https://fishshell.com/docs/3.1/cmds.html)