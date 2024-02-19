---
aliases:
- /nl/bash/handling-errors/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:59.158090-07:00
description: "Foutafhandeling in Bash-scripting gaat over het anticiperen op momenten\
  \ waarop dingen mis kunnen gaan en hier sierlijk mee omgaan. Waarom? Nou, het houdt\u2026"
lastmod: 2024-02-18 23:09:02.048961
model: gpt-4-0125-preview
summary: "Foutafhandeling in Bash-scripting gaat over het anticiperen op momenten\
  \ waarop dingen mis kunnen gaan en hier sierlijk mee omgaan. Waarom? Nou, het houdt\u2026"
title: Fouten afhandelen
---

{{< edit_this_page >}}

## Wat & Waarom?

Foutafhandeling in Bash-scripting gaat over het anticiperen op momenten waarop dingen mis kunnen gaan en hier sierlijk mee omgaan. Waarom? Nou, het houdt je script robuust en bespaart gebruikers hoofdbrekens wanneer dingen niet werken zoals verwacht.

## Hoe te:

```Bash
#!/bin/bash

# stderr omleiden naar een bestand
grep "iets" bestand.txt 2> fouten.log

# Foutafhandeling met exitstatussen
if ! grep "iets" bestand.txt; then
    echo "Oeps, er ging iets mis bij het zoeken naar 'iets'."
    exit 1
fi

# Een val opzetten om op te ruimen voor het verlaten na een fout
cleanup() {
  echo "Tijdelijke bestanden opruimen..."
  rm temp_*
}

trap cleanup ERR

# opzettelijke fout: bestand bestaat niet
cat temp_bestand.txt
```

Voorbeelduitvoer wanneer er een fout optreedt:

```
Tijdelijke bestanden opruimen...
cat: temp_bestand.txt: Bestand of map bestaat niet
```

## Diepgaand

Foutafhandeling in Bash-scripting gaat terug tot de oorsprong van de Unix-shell, waar robuuste en betrouwbare scripts (en nog steeds) vitaal zijn voor systeembeheer en automatisering. Traditioneel worden fouten in Bash afgehandeld door de exitstatus van een commando te controleren, die volgens conventie 0 teruggeeft voor succes en een niet-nulwaarde voor falen.

Bash introduceerde het commando `trap` als een ingebouwde functie, waarmee gebruikers commando's kunnen specificeren die uitgevoerd moeten worden op verschillende signalen of bij het verlaten van scripts. Dit is handig voor opruimtaken of een laatste redmiddel foutafhandelingsmechanisme.

Er is ook het commando `set`, dat het gedrag van Bash bij fouten kan veranderen. Bijvoorbeeld, `set -e` zal ervoor zorgen dat een script onmiddellijk stopt als een commando eindigt met een niet-nulstatus, een manier om snel te falen en opeenstapeling van fouten te vermijden.

Alternatieven voor ingebouwde foutafhandeling in Bash omvatten expliciet controleren op het bestaan van bestanden, commando-substitutie gebruiken, of zelfs je eigen functies schrijven om fouten meer gedetailleerd te behandelen.

Hoewel grondige foutafhandeling soms overbodig lijkt voor kleine scripts, is het een praktijk die veel tijd kan besparen bij het debuggen en onverwacht gedrag kan voorkomen voor zowel jou als de gebruikers.

## Zie Ook

- Bash-handleiding over Shell-parameters: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Geavanceerde Bash-Scriptinggidssectie over Foutafhandeling: https://www.tldp.org/LDP/abs/html/exit-status.html
- Een diepgaande gids voor `trap`: https://mywiki.wooledge.org/SignalTrap

Onthoud, scripten is een kunstvorm, en hoe je de misstappen en struikelingen afhandelt, kan je meesterwerk veerkrachtiger maken. Veel plezier met scripten!
