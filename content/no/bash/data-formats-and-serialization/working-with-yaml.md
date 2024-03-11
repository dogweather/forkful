---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:44.809594-07:00
description: "YAML, som st\xE5r for YAML Ain't Markup Language, er en menneskelesbar\
  \ standard for serialisering av data som kan brukes for konfigurasjonsfiler, samt\
  \ i\u2026"
lastmod: '2024-03-11T00:14:14.572264-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r for YAML Ain't Markup Language, er en menneskelesbar standard\
  \ for serialisering av data som kan brukes for konfigurasjonsfiler, samt i\u2026"
title: Arbeider med YAML
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML, som står for YAML Ain't Markup Language, er en menneskelesbar standard for serialisering av data som kan brukes for konfigurasjonsfiler, samt i applikasjoner hvor data lagres eller overføres. Programmerere trekkes mot YAML på grunn av dets klarhet og enkelhet, spesielt i prosjekter som innebærer komplekse konfigurasjoner eller behovet for lett redigerbare datastrukturer.

## Hvordan:

Å jobbe direkte med YAML i Bash krever litt oppfinnsomhet siden Bash ikke har innebygd støtte for parsing av YAML. Du kan imidlertid bruke eksterne verktøy som `yq` (en lett og bærbar kommandolinje YAML-prosessor) for å interagere effektivt med YAML-filer. La oss gå gjennom noen vanlige operasjoner:

### Installerer `yq`:

Før du dykker inn i eksemplene, forsikre deg om at du har `yq` installert. Du kan vanligvis installere det fra pakkebehandleren din, for eksempel på Ubuntu:

```bash
sudo apt-get install yq
```

Eller du kan laste det ned direkte fra GitHub-repositoriet.

### Lese en verdi:

Anta at du har en fil med navnet `config.yaml` med følgende innhold:

```yaml
database:
  vert: localhost
  port: 5432
bruker:
  navn: admin
  passord: hemmelig
```

For å lese databaseverten, kan du bruke `yq` som følger:

```bash
yq e '.database.vert' config.yaml
```

**Eksempel på utdata:**

```
localhost
```

### Oppdater en verdi:

For å oppdatere brukerens navn i `config.yaml`, bruk `yq eval`-kommandoen med `-i` (på stedet)-alternativet:

```bash
yq e '.bruker.navn = "nyadmin"' -i config.yaml
```

Verifiser endringen med:

```bash
yq e '.bruker.navn' config.yaml
```

**Eksempel på utdata:**

```
nyadmin
```

### Legge til et nytt element:

For å legge til et nytt element under databasedelen, som et nytt felt `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Å sjekke innholdet i filen vil bekrefte tillegget.

### Slette et element:

For å fjerne passordet under bruker:

```bash
yq e 'del(.bruker.passord)' -i config.yaml
```

Denne operasjonen vil fjerne passordfeltet fra konfigurasjonen.

Husk, `yq` er et kraftfullt verktøy og har mange flere muligheter, inkludert konvertering av YAML til JSON, sammenslåing av filer, og enda mer komplekse manipulasjoner. Henvis til `yq`-dokumentasjonen for ytterligere utforskning.
