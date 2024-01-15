---
title:                "Att stora en sträng"
html_title:           "Fish Shell: Att stora en sträng"
simple_title:         "Att stora en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#Varför

Att kapitalisera en sträng är en vanligt förekommande uppgift i programmering, särskilt när det kommer till formatering av text. Det kan hjälpa till att förbättra läsbarheten eller uppfylla vissa krav på specifika format. Genom att lära sig hur man kapitaliserar en sträng kan du förbättra din kodningsförmåga och möjliggöra mer flexibilitet i dina projekt.

##Hur du gör det

För att kapitalisera en sträng i Fish Shell, kan du använda funktionen "string capitalize". Detta gör att den första bokstaven i varje ord i strängen blir stor.

```Fish Shell
set name "karin" 
echo $name 
karin 
echo (string capitalize $name) 
Karin
```

Som du kan se i exemplet ovan blir "karin" ändrat till "Karin" med hjälp av funktionen "string capitalize". Du kan också lägga till ytterligare parametrar för att anpassa kapitaliseringsmönstret. Till exempel:

```Fish Shell
set name "karin" 
echo $name 
karin 
echo (string capitalize -s -p $name) 
KARIN
```

I det här exemplet har vi lagt till parametrarna "-s" och "-p" för att skapa en sträng som består av bara versaler. Det finns också andra parametrar du kan använda för att anpassa kapitaliseringsmönstret enligt dina preferenser.

##Djupdykning

För mer avancerade användare kan det vara värt att veta att Fish Shell använder sig av "POSIX Character Classes" för kapitaliseringsfunktionen. Detta innebär att funktionen kan använda olika språkinställningar för att korrekt kapitalisera strängar på olika språk.

För att använda en annan språkinställning kan du använda funktionen "LANG" tillsammans med "set -x" kommandot. Till exempel:

```Fish Shell
set -x LANG en_UK.UTF-8 
set name "karin" 
echo $name 
karin 
echo (string capitalize $name) 
Karin
```

Här används språkinställningen "en_UK.UTF-8" för att kapitalisera strängen "karin". Om du skulle byta till en annan språkinställning, som "sv_SE.UTF-8", skulle strängen kapitaliseras i enlighet med det språket.

##Se även

- Fish Shell dokumentation för "string capitalize": https://fishshell.com/docs/current/cmds/string-capitalize.html
- Information om "POSIX Character Classes": https://en.wikipedia.org/wiki/POSIX_character_classes
- Fish Shell Community: https://fishshell.com/community.html