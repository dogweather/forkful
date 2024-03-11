---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:59.860969-07:00
description: "Een debugger gebruiken draait allemaal om het pletten van bugs\u2014\
  de vervelende, tijdrovende fouten in je code. Programmeurs debuggen omdat ze problemen\u2026"
lastmod: '2024-03-11T00:14:25.090743-06:00'
model: gpt-4-0125-preview
summary: "Een debugger gebruiken draait allemaal om het pletten van bugs\u2014de vervelende,\
  \ tijdrovende fouten in je code. Programmeurs debuggen omdat ze problemen\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?
Een debugger gebruiken draait allemaal om het pletten van bugs—de vervelende, tijdrovende fouten in je code. Programmeurs debuggen omdat ze problemen efficiënt willen vinden en oplossen, de codeflow willen begrijpen en een duidelijker beeld willen krijgen van wat hun code daadwerkelijk doet.

## Hoe te:
Fish heeft geen ingebouwde debugger zoals sommige andere shells, maar je kunt externe hulpmiddelen zoals `gdb` gebruiken voor het debuggen van gecompileerde programma's of `fish -d` voor het uitvoeren van fish met debug output op verschillende niveaus. Laten we doorgaan met `fish -d`:

```fish
# Voer fish shell uit met debug niveau 2
fish -d2

# In de fish shell, laten we een simpele functie testen met een potentiele bug
function test_func
    set val 42
    echo "De waarde is $val"
    if test $val -eq 42
        echo "Alles is in orde."
    else
        echo "Er is iets fishy."
    end
end

# Roep de functie aan en observeer de debug output
test_func
```

Je zou extra debug output zien voor en na de uitvoering van de functie, dit helpt je bij het lokaliseren van problemen.

## Diepgaand
Historisch gezien is debuggen in Unix-achtige omgevingen het terrein geweest van gespecialiseerde hulpmiddelen zoals `gdb` voor C/C++ of `pdb` voor Python. In Fish ben je meestal afhankelijk van externe hulpprogramma's of ingebouwde functies zoals `functions -v` voor uitgebreide output van functies en `set -x` om variabele veranderingen te volgen.

Sommige mensen kiezen voor alternatieve shells zoals Bash vanwege functies zoals `set -x` voor het debuggen van scripts. Echter, Fish heeft zijn charme met een focus op gebruiksvriendelijkheid en interactiviteit, wat in veel gevallen de behoefte aan hardcore debuggen kan verminderen.

Wat betreft de implementatie, het debuggen van een script omvat vaak het uitvoeren ervan met uitgebreide output en het traceren van waar variabelen worden ingesteld, ongedaan gemaakt of op onverwachte manieren veranderd. Met Fish's kleurgecodeerde output en gebruiksvriendelijke benadering, kun je vaak het ingewikkelde debuggen vermijden - maar als je vastzit, onthoud dan dat verbositeit en duidelijkheid je beste hulpmiddelen zijn.

## Zie Ook
Hier zijn enkele betrouwbare reddingslijnen voor wanneer je tot je vinnen in de code zit:

- Fish documentatie over debuggen: https://fishshell.com/docs/current/index.html#debugging
- GDB (GNU Debugger) officiële gids: https://www.gnu.org/software/gdb/documentation/
- Stack Overflow Fish-tag - real-world debugging gevallen: https://stackoverflow.com/questions/tagged/fish
- Geavanceerde Bash-Scripting Gids - voor het vergelijken van debug benaderingen: https://tldp.org/LDP/abs/html/debugging.html
