---
title:                "Schrijven naar standaardfout"
date:                  2024-01-28T22:13:06.685688-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Schrijven naar standaardfout, `stderr`, geeft foutberichten apart van de standaarduitvoer, `stdout`. Programmeurs gebruiken `stderr` om fouten te rapporteren zonder in de weg te zitten van reguliere commando-uitvoeren, wat het gemakkelijker maakt om fouten te behandelen en te loggen.

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
