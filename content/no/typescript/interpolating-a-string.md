---
title:                "Interpolere en streng"
html_title:           "TypeScript: Interpolere en streng"
simple_title:         "Interpolere en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Interpolasjon av en streng er en måte å sette sammen en streng ved å bytte ut visse deler av den med variable verdier. Programmerere bruker dette for å lage dynamiske setninger eller meldinger ved å sette inn variabler i stedet for å skrive dem ut i klartekst.

# Hvordan:
Dette kan gjøres ved å bruke backticks (`) rundt en streng og plassere variabler inne i en krøllete parentes (${...}). Se eksemplet nedenfor for å se hvordan dette fungerer i TypeScript:

```TypeScript
let navn = "Kristoffer";
let alder = 25;
console.log(`Hei, jeg heter ${navn} og jeg er ${alder} år gammel.`);
```
Dette vil gi følgende utskrift:
```
Hei, jeg heter Kristoffer og jeg er 25 år gammel.
```

# Dypdykk:
Interpolasjon av en streng har en historie som strekker seg helt tilbake til Python-programmeringsspråket på 1990-tallet. Alternativene til interpolasjon inkluderer konkatenering av strenger og formateringsmetoder som sprintf(). I TypeScript, blir interpolasjon oversatt til en konkatenasjonsoperasjon som gjør koden mer effektiv og enkel å lese.

# Se også:
Les mer om interpolasjon av stringer i den offisielle TypeScript dokumentasjonen: https://www.typescriptlang.org/docs/handbook/strings.html#string-interpolation