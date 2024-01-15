---
title:                "Arbeid med yaml"
html_title:           "TypeScript: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor
YAML (YAML Ain't Markup Language) er et tekstbasert format som brukes til å lagre og utveksle data. Det er enklere å lese og skrive enn andre dataformater som JSON, og er derfor populært blant utviklere og systemadministratorer som ønsker en mer leservennlig måte å lagre data på.

## Slik gjør du det
For å kunne jobbe med YAML i TypeScript må du først installere en YAML-pakke fra npm. Det finnes flere ulike pakker å velge mellom, men en populær og godt dokumentert pakke er "js-yaml".

For å bruke "js-yaml" i et TypeScript-prosjekt, må du først installere pakken ved å kjøre følgende kommando i terminalen:
```TypeScript
npm install js-yaml
```

Deretter kan du importere pakken i TypeScript-filen din:
```TypeScript
import * as yaml from 'js-yaml';
```

For å lese en YAML-fil med "js-yaml", kan du bruke følgende kode:
```TypeScript
const fileContents = fs.readFileSync('data.yaml', 'utf8');
const data = yaml.load(fileContents);
console.log(data);
```

For å skrive til en YAML-fil med "js-yaml", kan du bruke følgende kode:
```TypeScript
const data = {
  name: 'John',
  age: 30,
  hobbies: ['programming', 'reading', 'hiking']
};

const yamlString = yaml.dump(data);
fs.writeFileSync('data.yaml', yamlString);
```

## Dypdykk
YAML er et hierarkisk format som er bygget opp av nøkler og verdier. Det brukes ofte til å konfigurere programmer og til å lagre datastrukturer. I TypeScript kan man arbeide med YAML ved hjelp av "js-yaml"-pakken, og få tilgang til alle nøkler og verdier i filen ved å bruke objektmetoden "load()". Man kan også konvertere JavaScript-objekter til YAML-format ved hjelp av "dump()" -metoden.

En praktisk funksjon ved YAML er at man kan inkludere andre YAML-filer i en eksisterende YAML-fil ved hjelp av "<<"-operatoren. Dette gjør det mulig å lage gjenbrukbare YAML-konfigurasjonsfiler som kan brukes i ulike programmer.

## Se også
- [js-yaml dokumentasjon](https://www.npmjs.com/package/js-yaml)
- [YAML spesifikasjon](https://yaml.org/spec/)
- [TypeScript dokumentasjon](https://www.typescriptlang.org/docs/home.html)