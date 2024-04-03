---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:49.382375-07:00
description: "Hoe te: In Fish kan loggen zo simpel zijn als het omleiden van standaard\
  \ uitvoer- en foutstromen naar een bestand. Laten we een logboekvermelding maken\u2026"
lastmod: '2024-03-13T22:44:51.253914-06:00'
model: gpt-4-0125-preview
summary: In Fish kan loggen zo simpel zijn als het omleiden van standaard uitvoer-
  en foutstromen naar een bestand.
title: Logboekregistratie
weight: 17
---

## Hoe te:
In Fish kan loggen zo simpel zijn als het omleiden van standaard uitvoer- en foutstromen naar een bestand. Laten we een logboekvermelding maken voor de start- en eindtijden van ons script.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script gestart" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script beëindigd" >> my_app.log
end

log_start
# ... de taken van je script ...
log_end

cat my_app.log
```

Dit is wat je zou zien in `my_app.log`:

```
2023-04-01 10:35:47  - Script gestart
2023-04-01 10:36:02  - Script beëindigd
```

Voor geavanceerd loggen, kun je functies met parameters voor logniveau en berichten gebruiken:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_niveau $argv[1]
    case '*'
      set log_niveau 'DEBUG'
  end
  set log_bericht (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_niveau]" $log_bericht >> my_app.log
end

log_message INFO "Dit is een informatief bericht."
log_message ERROR "Er is iets misgegaan!"
```

Voorbeeld `my_app.log` uitvoer zal zijn:
```
2023-04-01 10:35:47 [INFO] Dit is een informatief bericht.
2023-04-01 10:35:49 [ERROR] Er is iets misgegaan!
```

## Diepgaande Duik
Historisch gezien werd er in shell scripts gelogd met een hoop `echo`-statements, en hoewel dit zeker nog steeds een optie is, kan het implementeren van complexere systemen een uitdaging zijn. Fish heeft geen ingebouwd logmechanisme zoals sommige andere shells of programmeertalen dat hebben, dus vaak moet je je eigen systeem bedenken.

Alternatieven voor Fish's ingebouwde `echo`-commando voor loggen zijn Unix-tools zoals `syslog` of `logger`, die interface bieden met de systeemlog-daemon, wat een meer geïntegreerde aanpak biedt voor het loggen van systeembrede evenementen.

De eenvoud van Fish stelt je in staat om functies te creëren die de uitgebreidheid van loggen aanpakken, met verschillende niveaus die je aan of uit kunt zetten. Sommige implementaties kunnen zelfs de naam van het script, regelnummer en tijdstempel bevatten, waardoor het makkelijker wordt om terug te traceren door de stappen die tot een gebeurtenis hebben geleid.

## Zie Ook
- De Fish Shell Documentatie over het schrijven van functies: https://fishshell.com/docs/current/#syntax-function
- Basis Shell Scripting Tips: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Gids voor het Syslog Protocol: https://tools.ietf.org/html/rfc5424
