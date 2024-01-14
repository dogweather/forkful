---
title:                "Fish Shell: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en vanlig uppgift inom programmering och kan spara mycket tid och ansträngning när du behöver ändra flera instanser av samma text. Fish Shell erbjuder en enkel och kraftfull metod för att utföra detta, så att du kan fokusera på att skriva effektivare kod istället för att manuellt ersätta text.

## Hur man gör det
För att söka och ersätta text i Fish Shell, använd kommandot "sed" (Stream Editor). Detta kommando tar emot två parametrar: det första är ett reguljärt uttryck som specificerar vilken text som ska sökas efter, och det andra är ersättningstexten som ska byta ut den första texten. För att ersätta all förekomst av "hello" med "hej" i en fil kan du använda följande kommando:

```Fish Shell
sed 's/hello/hej/g' fil.txt
```

Det här kommandot använder "s" för att indikera att det är en sök- och ersättningsoperation, "g" för att utföra operationen på alla förekomster av texten och "fil.txt" för att ange vilken fil som ska ändras. Om du vill skriva över den befintliga filen med de nya ändringarna, lägg till flaggan "-i" efter "sed" kommandot.

## Djupdykning
Fish Shell använder "s" som standard för att indikera att det är en sök- och ersättningsoperation, men du kan också använda olika tecken som separatorer för att öka läsbarheten. Till exempel kan du använda "!" istället för "s" och "||" istället för "/" för att utföra samma operation som vi tidigare gjorde:

```Fish Shell
sed '!hello!||hej!g' fil.txt
```

Förutom att bara ersätta text, kan du faktiskt använda reguljära uttryck för att göra mer avancerad sökning och ersättning. Till exempel, om du vill ersätta alla siffror med "#", kan du använda detta kommando:

```Fish Shell
sed -E 's/[0-9]/#/g' fil.txt
```

Det här kommandot använder flaggan "-E" för att aktivera reguljära uttryck och "[0-9]" för att matcha alla siffror. Därefter byts varje siffra ut med "#" och operatören "g" används för att utföra operationen på alla förekomster av siffror i filen.

## Se även
* [Fish Shell Dokumentation](https://fishshell.com/docs/current/cmds/sed.html)
* [Reguljära uttryck i Fish Shell](https://fishshell.com/docs/current/index.html#regular-expressions)
* [5 användbara kommandon för text manipulation i Fish Shell](https://medium.com/@mahmoudalismail/writing-with-fish-shell-5-useful-commands-for-text-manipulation-ad2bc31a1663)