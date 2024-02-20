---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:25.775440-07:00
description: "Controleren of een map bestaat zorgt ervoor dat een programma werkt\
  \ met geldige paden, wat fouten voorkomt. Het is cruciaal voor taken die specifieke\u2026"
lastmod: 2024-02-19 22:05:10.349866
model: gpt-4-0125-preview
summary: "Controleren of een map bestaat zorgt ervoor dat een programma werkt met\
  \ geldige paden, wat fouten voorkomt. Het is cruciaal voor taken die specifieke\u2026"
title: Controleren of een directory bestaat
---

{{< edit_this_page >}}

## Wat & Waarom?
Controleren of een map bestaat zorgt ervoor dat een programma werkt met geldige paden, wat fouten voorkomt. Het is cruciaal voor taken die specifieke mappen nodig hebben, zoals het lezen van bestanden of het loggen van gegevens.

## Hoe:
Controleer of een map bestaat met een simpel `test` commando:
```Fish Shell
if test -d /pad/naar/map
    echo "Map bestaat"
else
    echo "Zulke map bestaat niet"
end
```
Voorbeelduitvoer wanneer de map bestaat:
```
Map bestaat
```
Voorbeelduitvoer wanneer de map niet bestaat:
```
Zulke map bestaat niet
```

## Diepgaand
Het `test` commando (`[ ]` in POSIX shells) is al decennia lang onderdeel van Unix-achtige systemen. In Fish controleert `test -d` op het bestaan van een map. Dit is een betere aanpak dan vertrouwen op uitvoer van commando's zoals `ls`, die inconsistent of omslachtig kunnen zijn.

Alternatieven:
- `status` kan bepalen of een vorig commando, zoals `cd /pad/naar/map`, succesvol was. Echter, dit wordt niet aanbevolen puur voor bestaanscontroles, aangezien het de staat van de shell verandert.
- Externe gereedschappen zoals `find` of scripttalen (Python, Ruby) kunnen vergelijkbare taken uitvoeren, maar zijn vaak teveel van het goede voor simpele controles.

Implementatiedetails:
Fish's ingebouwde `test` commando is efficiënt en betrouwbaar. Het vermijdt veel voorkomende valkuilen bij het aanroepen van externe commando's en biedt een eenvoudige syntax.

## Zie Ook

- Documentatie van Fish Shell over `test`: https://fishshell.com/docs/current/cmds/test.html
- POSIX specificatie voor `test`: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html
- Discussie over het controleren van bestaan van bestanden: https://unix.stackexchange.com/questions/590694/checking-if-a-directory-exists-in-unix-shell-scripting
