---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:22.385220-07:00
description: "Hvordan: Fish Shell har ikke innebygd st\xF8tte for parsing av YAML,\
  \ men du kan benytte tredjepartsverkt\xF8y som `yq` (en lettvektig og b\xE6rbar\
  \ kommandolinje\u2026"
lastmod: '2024-03-13T22:44:41.247830-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell har ikke innebygd st\xF8tte for parsing av YAML, men du kan benytte\
  \ tredjepartsverkt\xF8y som `yq` (en lettvektig og b\xE6rbar kommandolinje YAML-prosessor)\
  \ for \xE5 h\xE5ndtere YAML-data."
title: Arbeider med YAML
weight: 41
---

## Hvordan:
Fish Shell har ikke innebygd støtte for parsing av YAML, men du kan benytte tredjepartsverktøy som `yq` (en lettvektig og bærbar kommandolinje YAML-prosessor) for å håndtere YAML-data.

**Installasjon av yq (hvis ikke allerede installert):**
```fish
sudo apt-get install yq
```

**Lese en verdi fra en YAML-fil:**
Anta at du har en YAML-fil `config.yaml` med følgende innhold:
```yaml
database:
  host: localhost
  port: 3306
```

For å lese databaseverten, ville du brukt:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Eksempel på output:**
```
localhost
```

**Oppdatere en verdi i en YAML-fil:**
For å oppdatere `port` til `5432`, bruk:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Verifisere oppdateringen:**
```fish
yq e '.database.port' config.yaml
```
**Eksempel på output:**
```
5432
```

**Skrive en ny YAML-fil:**
For å opprette en ny `new_config.yaml` med forhåndsdefinert innhold:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Dette bruker `yq` for å behandle og pene utskrift (-P-flagg) en streng til en ny YAML-fil.

**Parse komplekse strukturer:**
Hvis du har en mer kompleks YAML-fil og trenger å hente nestleder arrayer eller objekter, kan du:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Eksempel på output:**
```
server1
server2
```
Ved bruk av `yq`, gjør Fish Shell det greit å navigere gjennom YAML-dokumenter og manipulere dem for ulike automatiserings- og konfigurasjonsoppgaver.
