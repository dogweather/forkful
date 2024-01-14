---
title:                "Javascript: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML er et populært filformat for konfigurasjonsfiler og metadata. Det er enkelt å lese og skrive for mennesker, noe som gjør det til et flott valg for å organisere og formatere data. Det gir også mulighet for å lage komplekse hierarkiske strukturer, noe som gjør det enkelt å håndtere store datasett.

## Hvordan

For å begynne å jobbe med YAML i JavaScript, må du først installere en parser som forstår formatet. Et godt alternativ er js-yaml, som kan installeres ved hjelp av npm-kommandoen ```npm install js-yaml```.

Når pakken er installert, kan du begynne å bruke den i koden din ved å først importere den:

```Javascript
const yaml = require('js-yaml');
```

Deretter kan du bruke ```load()```-funksjonen til å lese en YAML-fil og konvertere den til et JavaScript-objekt:

```Javascript
const data = yaml.load('data.yml');
```

Du kan også bruke ```dump()```-funksjonen for å konvertere et JavaScript-objekt til YAML-format og skrive det til en fil:

```Javascript
const data = {
  name: 'John Doe',
  age: 28
};

yaml.dump(data, 'data.yml');
```

## Dypdykk

En av de mest nyttige egenskapene ved YAML er at det støtter hierarkiske strukturer. Dette betyr at du kan organisere dataene dine på en logisk måte ved å bruke innrykk. For eksempel:

```YAML
name: John Doe
age: 28
address:
  street: Main Street
  city: Oslo
  country: Norway
```

Det er også mulig å definere egendefinerte datatyper og referere til dem i YAML-filer ved hjelp av anker og alias. Dette gjør det enklere å gjenbruke og vedlikeholde kode.

For en mer utfyllende og detaljert innføring i YAML, kan du sjekke ut dokumentasjonen på [YAMLs offisielle nettside](https://yaml.org/) eller [js-yaml's GitHub-repository](https://github.com/nodeca/js-yaml).

## Se også

- [YAML offisiell nettside](https://yaml.org/)
- [js-yaml GitHub-repository](https://github.com/nodeca/js-yaml)
- [npm pakken for js-yaml](https://www.npmjs.com/package/js-yaml)