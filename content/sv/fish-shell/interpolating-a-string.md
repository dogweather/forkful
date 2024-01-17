---
title:                "Interpolering av strängar"
html_title:           "Fish Shell: Interpolering av strängar"
simple_title:         "Interpolering av strängar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolering av en sträng är när man sätter in variabler eller uttryck i en sträng för att skapa en dynamiskt genererad text. Detta är användbart för att skapa mer flexibla och anpassningsbara utskrifter eller meddelanden i ett program.

Programmerare använder interpolering för att kunna inkludera variabler eller resultat av uttryck i en sträng utan att behöva hålla på med konkatenering eller andra manipuleringar av strängen. Det sparar tid och gör koden mer läsbar och underhållbar.

## Hur man:
Fish Shell har inbyggd funktion för stränginterpolering med hjälp av syntaxen `"$variabel"` eller `"$((uttryck))"`. Variabler och uttryck kan sättas in i en sträng med hjälp av `echo` eller genom att helt enkelt skriva ut strängen.

```Fish Shell
set namn "Sven"
set ålder 35
echo "Hej $namn, du är $ålder år gammal!"
```
Output: `Hej Sven, du är 35 år gammal!`

Det går också att sätta in flera variabler eller uttryck i en sträng och kombinera dem på olika sätt. Här är ett exempel där vi sätter in variabler i en tabell och sedan loopar igenom dem för att skriva ut en sträng för varje variabel:

```Fish Shell
set var1 "Hej"
set var2 "på"
set var3 "dig"
set var4 "!"
for var in $var1 $var2 $var3 $var4
    echo "$var"
end
```
Output: `Hej`, `på`, `dig`, `!`

## Djupdykning:
Historiskt sett, har interpolering av strängar funnits sedan tidigt 1970-tal och användes ursprungligen i programmeringsspråken Snobol och Icon. Idag finns det flera olika språk och programmeringsverktyg som har stöd för interpolering av strängar, inklusive Fish Shell.

En vanlig alternativ till syntaxen som används i Fish Shell är att använda `{}` runt variabler istället för `"$"`. Det finns också specifika funktioner inom Fish Shell som kan användas för att generera formaterade strängar eller för att manipulera text på andra sätt.

## Se även:
Officiell dokumentation för Fish Shell:s syntax för interpolering av strängar: https://fishshell.com/docs/current/cmds/set.html#interpolated-strings

En guide till interpolering av strängar i andra programmeringsspråk: https://blog.logrocket.com/string-interpolation-in-code-a-how-to-guide/

En diskussion om fördelarna och nackdelarna med olika syntax för interpolering av strängar: https://stackoverflow.com/questions/36817758/when-to-use-string-with-variable-as-opposed-to-variable-with-string