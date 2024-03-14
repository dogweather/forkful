---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:06.850180-07:00
description: "YAML, som st\xE5r f\xF6r YAML Ain't Markup Language, \xE4r en l\xE4\
  sbar dataseringsstandard som kan anv\xE4ndas f\xF6r konfigurationsfiler, s\xE5v\xE4\
  l som i applikationer d\xE4r\u2026"
lastmod: '2024-03-13T22:44:38.102372-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r f\xF6r YAML Ain't Markup Language, \xE4r en l\xE4sbar\
  \ dataseringsstandard som kan anv\xE4ndas f\xF6r konfigurationsfiler, s\xE5v\xE4\
  l som i applikationer d\xE4r\u2026"
title: Att Arbeta med YAML
---

{{< edit_this_page >}}

## Vad & Varför?

YAML, som står för YAML Ain't Markup Language, är en läsbar dataseringsstandard som kan användas för konfigurationsfiler, såväl som i applikationer där data lagras eller överförs. Programmerare dras till YAML på grund av dess klarhet och enkelhet, särskilt i projekt som innefattar komplexa konfigurationer eller behovet av lätt redigerbara datastrukturer.

## Hur man gör:

Att arbeta direkt med YAML i Bash kräver lite påhittighet eftersom Bash inte har inbyggt stöd för att tolka YAML. Du kan dock använda externa verktyg som `yq` (en lätt och bärbar kommandorads-YAML-processor) för att effektivt interagera med YAML-filer. Låt oss gå igenom några vanliga operationer:

### Installera `yq`:

Innan du dyker in i exemplen, se till att du har `yq` installerat. Du kan vanligtvis installera det från din pakethanterare, till exempel på Ubuntu:

```bash
sudo apt-get install yq
```

Eller du kan ladda ner det direkt från dess GitHub-repositorium.

### Läsa ett värde:

Anta att du har en fil med namnet `config.yaml` med följande innehåll:

```yaml
databas:
  värd: localhost
  port: 5432
användare:
  namn: admin
  lösenord: hemlighet
```

För att läsa databasvärdet kan du använda `yq` så här:

```bash
yq e '.databas.värd' config.yaml
```

**Exempelutdata:**

```
localhost
```

### Uppdatera ett värde:

För att uppdatera användarens namn i `config.yaml`, använd kommandot `yq eval` med alternativet `-i` (på plats):

```bash
yq e '.användare.namn = "nyadmin"' -i config.yaml
```

Verifiera ändringen med:

```bash
yq e '.användare.namn' config.yaml
```

**Exempelutdata:**

```
nyadmin
```

### Lägga till ett nytt element:

För att lägga till ett nytt element under databasavsnittet, som ett nytt fält `timeout`:

```bash
yq e '.databas.timeout = 30' -i config.yaml
```

Kontroll av innehållet i filen kommer att bekräfta tillägget.

### Ta bort ett element:

För att ta bort lösenordet under användare:

```bash
yq e 'del(.användare.lösenord)' -i config.yaml
```

Denna operation kommer att ta bort lösenordsfältet från konfigurationen.

Kom ihåg, `yq` är ett kraftfullt verktyg och har mycket mer kapaciteter, inklusive konvertering av YAML till JSON, sammanfogning av filer och ännu mer komplexa manipuleringar. Hänvisa till `yq`-dokumentationen för vidare utforskning.
