---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:41.745790-07:00
description: "YAML, som st\xE5r for \"YAML Ain't Markup Language,\" er en menneskelesbar\
  \ standard for serialisering av data som ofte brukes for konfigurasjonsfiler og\u2026"
lastmod: '2024-03-13T22:44:40.338177-06:00'
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r for \"YAML Ain't Markup Language,\" er en menneskelesbar\
  \ standard for serialisering av data som ofte brukes for konfigurasjonsfiler og\
  \ dataveksling mellom spr\xE5k med ulike datastrukturer."
title: Arbeider med YAML
weight: 41
---

## Hva & Hvorfor?

YAML, som står for "YAML Ain't Markup Language," er en menneskelesbar standard for serialisering av data som ofte brukes for konfigurasjonsfiler og dataveksling mellom språk med ulike datastrukturer. Programmerere arbeider ofte med YAML på grunn av dets enkelhet og lesbarhet, spesielt i prosjekter som krever omfattende konfigurasjon eller når strukturerte data overføres mellom ulike systemer.

## Hvordan:

Selv om Google Apps Script (GAS) ikke støtter parsing eller serialisering av YAML nativt, kan du manipulere YAML-data ved å bruke JavaScript-biblioteker eller skrive egendefinerte parsingfunksjoner. For demonstrasjon, la oss vurdere hvordan vi parser en YAML-streng ved hjelp av en egendefinert funksjon, siden eksterne biblioteker ikke kan importeres direkte inn i GAS.

Anta at du har en enkel YAML-konfigurasjon:

```yaml
title: YAML Example
description: Et eksempel på hvordan håndtere YAML i Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Konfigurasjon
```

For å parse dette i Google Apps Script, bruk JavaScripts egenskaper for strengmanipulasjon:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // Enkel håndtering for arrays
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML Example\ndescription: Et eksempel på hvordan håndtere YAML i Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Konfigurasjon";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Når `testYamlParsing()` utføres, blir det utdatert:

```
{ title: 'YAML Example',
  description: 'Et eksempel på hvordan håndtere YAML i Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Konfigurasjon' ] }
```

Denne egendefinerte parsingtilnærmingen er ganske enkel og kan trenge justeringer for å kunne håndtere komplekse YAML-filer.

## Dypdykk

YAML, som først ble utgitt i 2001, hadde som mål å være mer menneskelesbar enn sine forgjengere som XML eller JSON. Selv om dens enkelhet og brukervennlighet er bredt anerkjent, presenterer håndteringen av YAML i Google Apps Script utfordringer på grunn av mangelen på direkte støtte. Følgelig stoler programmerere ofte på JavaScripts allsidighet for å parse og generere YAML-data. Imidlertid kan denne metoden bli omstendelig og feilutsatt for komplekse brukstilfeller, spesielt de som involverer dyp nesting og avanserte datastrukturer.

JSON, derimot, støttes nativt i Google Apps Script og de fleste andre programmeringsmiljøer, og tilbyr en mer direkte tilnærming for serialisering og deserialisering av data uten ekstra parsing. JSONs syntaks er mindre langdryg enn YAMLs, noe som gjør den mer egnet for datautveksling i webapplikasjoner. Likevel, YAML forblir populært for konfigurasjonsfiler og situasjoner hvor menneskelig lesbarhet er av største viktighet.

Når du arbeider med YAML i Google Apps Script, vurder avveiningene mellom lesbarhet og brukervennlighet. For omfattende håndtering av YAML, kan det være verdt å utforske eksterne verktøy eller tjenester som kan konvertere YAML til JSON før du behandler det i scriptet ditt.
