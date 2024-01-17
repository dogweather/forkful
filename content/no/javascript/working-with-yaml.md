---
title:                "Arbeid med yaml"
html_title:           "Javascript: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML står for "YAML Ain't Markup Language" og er et format for å representere data i en lesbar og strukturert form. Det er ofte brukt av programmerere for å konfigurere og lagre data som blir brukt i deres applikasjoner.

## Hvordan å:

```Javascript
// Opprette et YAML-dokument
const yaml = require('js-yaml');

var employee = {
  name: 'John Doe',
  age: 30,
  role: 'developer'
};

var yamlDoc = yaml.safeDump(employee);
console.log(yamlDoc);
```

```
Output:
name: John Doe
age: 30
role: developer
```

## Dypdykk:

YAML er inspirert av programmeringsspråket Python sin stil og er designet for å være menneskeleselig og enkelt å forstå. Det er ofte brukt i webapplikasjoner for å konfigurere og lagre innstillinger, og har blitt et populært valg for å erstatte XML som et format for strukturert data.

Et alternativ til YAML er JSON, som også er et populært valg for å representere data. Imidlertid er YAML mer menneskeleselig og kan være mer brukervennlig for ikke-tekniske personer.

Implementeringen av YAML i Javascript er enkelt med bibliotek som JS-YAML og yaml.js, som lar deg lese og skrive YAML-formatert data i dine programmer.

## Se også:

- [JS-YAML Bibliotek](https://www.npmjs.com/package/js-yaml)
- [yaml.js Bibliotek](https://www.npmjs.com/package/yamljs)
- [YAML offisiell nettside](https://yaml.org/)