---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:41.342767-07:00
description: "Att arbeta med YAML inneb\xE4r att tolka och hantera YAML-filer (YAML\
  \ Ain't Markup Language), ett serialiseringsformat f\xF6r data som anv\xE4nds f\xF6\
  r\u2026"
lastmod: '2024-03-13T22:44:38.358542-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med YAML inneb\xE4r att tolka och hantera YAML-filer (YAML Ain't\
  \ Markup Language), ett serialiseringsformat f\xF6r data som anv\xE4nds f\xF6r konfigurationsfiler,\
  \ i Fish Shell."
title: Att Arbeta med YAML
weight: 41
---

## Hur man gör:
Fish Shell har inte inbyggt stöd för att tolka YAML, men du kan använda tredjepartsverktyg som `yq` (en lätt och bärbar kommandorads YAML-processor) för att hantera YAML-data.

**Installation av yq (om det inte redan är installerat):**
```fish
sudo apt-get install yq
```

**Läsa ett värde från en YAML-fil:**
Anta att du har en YAML-fil `config.yaml` med följande innehåll:
```yaml
database:
  host: localhost
  port: 3306
```

För att läsa databasvärdet använder du:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Exempel på utdata:**
```
localhost
```

**Uppdatera ett värde i en YAML-fil:**
För att uppdatera `port` till `5432`, använd:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Verifiera uppdateringen:**
```fish
yq e '.database.port' config.yaml
```
**Exempel på utdata:**
```
5432
```

**Skriva en ny YAML-fil:**
För att skapa en ny `new_config.yaml` med fördefinierat innehåll:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Detta använder `yq` för att bearbeta och snygga till (-P flaggan) en sträng till en ny YAML-fil.

**Tolka komplexa strukturer:**
Om du har en mer komplex YAML-fil och behöver hämta nästlade arrayer eller objekt, kan du:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Exempel på utdata:**
```
server1
server2
```
Med hjälp av `yq` gör Fish Shell det enkelt att navigera genom YAML-dokument och manipulera dem för olika automatiserings- och konfigureringsuppgifter.
