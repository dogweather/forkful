---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:06.685688-07:00
description: 'Hoe te: Voorbeelduitvoer.'
lastmod: '2024-04-05T21:53:51.018198-06:00'
model: gpt-4-0125-preview
summary: ''
title: Schrijven naar standaardfout
weight: 25
---

## Hoe te:
```
# Het echo-commando omleiden naar standaardfout
echo "Fout: Ongeldige invoer." >&2

# Met printf schrijven naar standaardfout
printf "Fout: Bestand niet gevonden.\n" >&2

# Voorbeeldscript dat naar zowel stdout als stderr schrijft
echo "Proces starten..."
echo "Oeps! Er ging iets fout." >&2
echo "Proces voltooid."
```
Voorbeelduitvoer:
```
Proces starten...
Proces voltooid.
Oeps! Er ging iets fout.
```
In het bovenstaande wordt "Oeps! Er ging iets fout." naar `stderr` gestuurd en kan het in een terminal buiten volgorde verschijnen wanneer het wordt gemengd met `stdout`, aangezien `stderr` doorgaans niet gebufferd is.

## Diepgaande duik
Bash erft het concept van 'bestandsdescriptors' van Unix, met `stdout` naar fd `1` en `stderr` naar fd `2`. Omleiden naar `&2` stuurt de uitvoer naar `stderr`. Historisch gezien maakt deze scheiding het beheer en het filteren gemakkelijker, met `2>&1` als een veelgebruikt patroon om `stderr` naar `stdout` om te leiden. Een levensvatbaar alternatief voor expliciete omleiding is het gebruik van `logger` voor syslog-integratie of het configureren van het script om fouten intern te behandelen.

## Zie Ook
- Spiekbriefje voor Bash-omleidingen: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Redirections
- Uitgebreid overzicht van Bash-scripting: https://www.tldp.org/LDP/Bash-Beginners-Guide/html/
- Geavanceerde Bash-scriptinggids: https://www.tldp.org/LDP/abs/html/
