---
title:                "Organisera kod i funktioner"
date:                  2024-01-28T23:01:28.693984-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organisera kod i funktioner"

category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att organisera kod i funktioner handlar om att bunta ihop bitar av skript för att göra specifika uppgifter. Vi gör det eftersom det gör koden enklare att läsa, testa och återanvända — ingen vill trassla igenom ett träsk av kodspaghetti.

## Hur man gör:
I Fish skriver du en funktion med `function`-nyckelordet, ger den ett namn, och avslutar med `end`. Här är en enkel:

```fish
function hello
    echo "Hello, World!"
end

hello
```

Utskrift:
```
Hello, World!
```

Nu, låt oss göra den hälsa på en användare:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

Utskrift:
```
Hey there, ditt_användarnamn!
```

För att spara den över sessioner, använd `funcsave greet`.

## Fördjupning
Fish Shell-funktioner är som mini-script — du kan stoppa in nästan vad som helst där. Historiskt sett har begreppet funktioner i shellskriptning sparat otaliga timmar av repetitivt skrivande och felsökning. Till skillnad från programmeringsspråk som Python, handlar Shell-funktioner mer om bekvämlighet än struktur.

Vissa skal, som Bash, använder `function` eller bara raka klammerparenteser. Fish håller sig till `function ... end` — tydligt och lättläst. Inuti Fish-funktioner får du alla finesser: parametrar, lokala variabler med `set -l`, och du kan till och med definiera en funktion inuti en annan funktion.

Du behöver inte ett `return`-värde eftersom Fish inte fokuserar mycket på det; utdatan från din funktion är dess returvärde. Och om du vill ha beständiga funktioner tillgängliga för framtida sessioner, kom ihåg `funcsave`.

## Se även

- Fish-tutorialen om funktioner: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Funktionskommandon

- [function](https://fishshell.com/docs/current/cmds/function.html) — Skapa en funktion
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Skriv ut eller radera funktioner
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Spara definitionen av en funktion till användarens autoload-katalog
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Redigera en funktion interaktivt
