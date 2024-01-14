---
title:                "Ruby: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML (Yet Another Markup Language) er et populært format for å konfigurere og lagre data i en tekstbasert fil. Det er lett å lese og forstå, og brukes ofte til å lage konfigurasjonsfiler for programmer og nettsider. Ved å lære hvordan man arbeider med YAML, kan man enkelt organisere og lagre data på en strukturert og effektiv måte.

## Hvordan å arbeide med YAML i Ruby

For å kunne arbeide med YAML i Ruby, må man først sørge for at man har installert YAML-pakken ved å kjøre følgende kommando i terminalen: 

```
gem install yaml
```

Nå kan vi begynne å importere YAML-modulen og bruke dens funksjoner. Her er et eksempel på hvordan man kan lese inn data fra en YAML-fil og konvertere det til et Ruby-objekt:

```
require 'yaml'

# Leser inn YAML-filen
yaml_data = YAML.load_file('data.yml')

# Gjør om til Ruby-objekt
ruby_data = YAML.load(yaml_data)

# Printer ut data
puts ruby_data
```

Dette vil skrive ut dataene som ble lagret i YAML-filen, i en strukturert form som kan leses av Ruby.

Man kan også bruke YAML til å lagre data i en fil. Her er et eksempel på hvordan man lagrer en liste med navn i en YAML-fil:

```
require 'yaml'

# Oppretter en liste med navn
names = ["Marius", "Ingrid", "Nora", "Oscar"]

# Konverterer til YAML
yaml_data = names.to_yaml

# Skriver til fil
File.open('names.yml', 'w') {|file| file.write(yaml_data) }
```

Dette vil resultere i en fil kalt "names.yml" med innholdet:

```
--- 
- Marius
- Ingrid
- Nora
- Oscar
```

Man kan også bruke YAML til å legge til kommentarer i sine konfigurasjonsfiler. Dette gjøres ved å bruke "#" foran kommentaren. For eksempel:

```
# Dette er en kommentar
navn: Marius
```

## Dykk dypere

Ved å dykke dypere i YAML, kan man oppdage mer komplekse og kraftige funksjoner. Det finnes for eksempel mulighet for å koble sammen forskjellige YAML-filer, bruke variabler, og lage egendefinerte datatyper. Det finnes også mange tilleggsbiblioteker og verktøy som bygger videre på YAML og gjør det enda enklere å arbeide med.

Det er viktig å huske på at YAML er et tekstbasert format, og derfor er det viktig å sørge for riktig formatering og syntaks for å unngå feil.

## Se også

- [YAML offisiell nettside] (https://yaml.org/)
- [YAML spesifikasjon] (https://yaml.org/spec/)
- [YAML i Ruby dokumentasjon] (https://ruby-doc.org/stdlib-2.6.3/libdoc/yaml/rdoc/YAML.html)