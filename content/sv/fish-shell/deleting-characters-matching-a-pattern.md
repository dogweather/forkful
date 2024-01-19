---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att radera tecken som matchar en viss mönster är en frekvent operation i programmering. Detta används för att städa upp data, så som strängmanipulation, för att bara behålla den information som är av intresse.

## Hur man:
För att visa hur man tar bort tecken i strängar som matchar ett visst mönster i Fish, kommer vi att använda `string match`-kommandot.

```Fish Shell
set sträng "Hej värd!"
set ny_sträng (string match -rv ' ' $sträng)
echo $ny_sträng
```
Resultatet blir:
```Fish Shell
Hejvärd!
```
I exemplet ovan tar vi bort mellanslag från strängen "Hej värd!".

## Djupdykning:
Funktionen för att matcha och ta bort tecken från strängar har varit en del av Unix och Linux-programmeringsmiljön från början, och har sedan infört i Fish-Shell. 

Ett alternativ till 'string match' användning är att använda `sed/använda`-kommandot för att utföra en liknande uppgift. 

```Fish Shell
echo Hej värd! | sed 's/ //g'
```

Vad gäller implementeringsdetaljerna är det viktigt att notera att Fish använder en matchande algoritm som är känslig för stora och små bokstäver, om inte annat anges.

## Se Även:
Mer information och exempel på 'string match' kan hittas i Fish Shells officiella dokumentation:
* [Fish Shell 'string' dokumentation](https://fishshell.com/docs/current/cmds/string.html)

Också, här är länkar till alternativ tekniker:
* [Sed/Använda Linux-kommando](https://www.gnu.org/software/sed/manual/sed.html)