---
title:                "Att organisera kod i funktioner"
date:                  2024-01-26T01:10:28.078409-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att organisera kod i funktioner"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att organisera kod i funktioner handlar om att bunta ihop delar av skript för att utföra specifika uppgifter. Vi gör det eftersom det gör koden lättare att läsa, testa och återanvända – ingen vill trassla sig igenom en sumpmark av spaghetti-kod.

## Hur man gör:
I Fish skriver du en funktion med `function`-nyckelordet, ger den ett namn och avslutar med `end`. Här är en enkel sådan:

```fish
function hello
    echo "Hej, världen!"
end

hello
```

Utskrift:
```
Hej, världen!
```

Nu låt oss göra så att den hälsar på en användare:

```fish
function greet
    set user (whoami)
    echo "Hej där, $user!"
end

greet
```

Utskrift:
```
Hej där, ditt_användarnamn!
```

För att spara den över sessioner, använd `funcsave greet`.

## Fördjupning
Fish Shell-funktioner är som mini-skript — du kan stoppa i nästan vad som helst där. Historiskt sett har konceptet med funktioner i shellskript sparat otaliga timmar av repetitivt skrivande och felsökning. Till skillnad från programmeringsspråk som Python, handlar Shell-funktioner mer om bekvämlighet än struktur.

Vissa skal, som Bash, använder `function` eller bara raka klamrar. Fish håller sig till `function ... end`— tydligt och lättläst. Inuti Fish-funktioner har du alla finesser: parametrar, lokala variabler med `set -l`, och du kan till och med definiera en funktion inuti en annan funktion.

Du behöver inte något `return`-värde eftersom Fish inte är stort på det; din funktions utdata är dess retur. Och om du vill ha beständiga funktioner tillgängliga för framtida sessioner, kom ihåg `funcsave`.

## Se också
- Fish-tutorialen om funktioner: https://fishshell.com/docs/current/tutorial.html#tut_functions
- Fish-dokumentationen för `function`: https://fishshell.com/docs/current/cmds/function.html
- En omfattande guide om att skriva funktioner i fish: https://fishshell.com/docs/current/index.html#syntax-function