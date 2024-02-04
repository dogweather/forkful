---
title:                "Att Arbeta med YAML"
date:                  2024-02-03T19:25:41.342767-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med YAML innebär att tolka och hantera YAML-filer (YAML Ain't Markup Language), ett serialiseringsformat för data som används för konfigurationsfiler, i Fish Shell. Programmerare gör detta för att automatisera och konfigurera applikationer eller tjänster effektivt inom ramen för skal-miljöer, vilket underlättar uppgifter som konfigurationshantering och tillhandahållande av resurser.

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
