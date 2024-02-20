---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:32.005929-07:00
description: "YAML ain't Markup Language (YAML) is een voor mensen leesbare standaard\
  \ voor gegevensserialisatie. Programmeurs gebruiken het voor configuratiebestanden,\u2026"
lastmod: 2024-02-19 22:05:10.088003
model: gpt-4-0125-preview
summary: "YAML ain't Markup Language (YAML) is een voor mensen leesbare standaard\
  \ voor gegevensserialisatie. Programmeurs gebruiken het voor configuratiebestanden,\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?
YAML ain't Markup Language (YAML) is een voor mensen leesbare standaard voor gegevensserialisatie. Programmeurs gebruiken het voor configuratiebestanden, gegevensopslag en interprocesberichtgeving vanwege de eenvoud en leesbaarheid.

## Hoe te:
Hier is een eenvoudig voorbeeld van het lezen van een YAML-bestand met Bash.

Gegeven `config.yaml`:
```yaml
database:
  host: localhost
  port: 3306
  gebruikersnaam: gebruiker
  wachtwoord: pass123
```

Gebruik dit script om de YAML te lezen en de databasehost te printen:

```Bash
#!/bin/bash
waarde=$(grep 'host:' config.yaml | awk '{ print $2 }')
echo "Database Host: ${waarde}"
```

Voorbeelduitvoer:
```
Database Host: localhost
```

## Diepduiken
YAML, gecreëerd in 2001, is een meer mensvriendelijk alternatief voor XML of JSON. Het wordt veel gebruikt in cloudservices, app-implementatie en devops-tools. Hoewel Bash geen native YAML-parsering heeft, kunnen tools zoals `yq` en parseren via `awk` of `grep` het werk doen. Echter, complexe parsing kan behoorlijk YAML-tooling vereisen.

## Zie Ook
- Officiële YAML-website: https://yaml.org
- `yq`, een opdrachtregel YAML-processor: https://github.com/kislyuk/yq
- Bash YAML-parsing discussie: https://stackoverflow.com/questions/5014632/how-can-i-parse-a-yaml-file-from-a-linux-shell-script
