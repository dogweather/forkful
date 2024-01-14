---
title:    "Fish Shell: Utvinna delsträngar"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Om du stöter på en textsträng i ditt Fish Shell-program och bara vill ha en del av den, så kan du använda substrings för att extrahera den specifika delen. Detta kan hjälpa till med bearbetning av text som kräver ett visst format eller innehåller önskat data i en viss del av strängen.

## Hur man gör

Det finns flera sätt att extrahera substrings i Fish Shell. Ett sätt är att använda kommandot `string sub` och ange start- och slutpositionen för det önskade substringet.

```Fish Shell
set str "Hej världen!"
echo (string sub -1 -1 $str)
```

Det ovanstående exemplet kommer att extrahera sista tecknet i strängen "Hej världen!" och visa det som output, vilket i detta fall är ett utropstecken.

En annan metod är att använda `string match` kommandot för att matcha ett mönster och extrahera substrings baserat på detta mönster.

```Fish Shell
set str "Min favoritfärg är blå."
echo (string match -r "[fF]ärg \\w+" $str)
```

I det här exemplet kommer vi att matcha och extrahera ordet efter "färg", som i detta fall är "blå". Det är också värt att notera att vi har använt flaggan `-r` för att göra en regular expression-sökning.

## Djupare granskning

Vid användning av `string sub` kommandot, kan du också ange en stegstorlek för att extrahera substrings med en viss avstånd från det angivna området. Till exempel, om du bara vill extrahera varannan bokstav från en sträng, kan du använda `string sub` tillsammans med en stegstorlek på 2.

```Fish Shell
set str "abcdefg"
echo (string sub 1 6 2 $str)
```

Detta kommer att extrahera varannan bokstav från index 1 till 6, vilket i detta fall är "bdf".

Fish Shell har också ett inbyggt kommando `cut` som också kan användas för att extrahera substrings från en sträng. Detta kommando är dock inkonsekvent när det gäller att hantera start- och slutpositioner, så det är bäst att använda andra metoder för att extrahera substrings.

## Se även

- [Fish Shell-dokumentation för substrings](https://fishshell.com/docs/current/cmds/string.html#substring-functions)
- [Bash-begrepp: substrings](https://bash.cyberciti.biz/guide/Substring_extraction)