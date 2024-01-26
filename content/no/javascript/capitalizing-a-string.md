---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Store bokstaver i en streng betyr å gjøre den første bokstaven i hvert ord til en stor bokstav. Vi gjør dette for å følge typografiske normer, forbedre lesbarheten eller formatere tekst etter visse krav, for eksempel i titler eller overskrifter.

## Hvordan:
```javascript
function capitalizeString(str) {
  return str.replace(/(?:^|\s)\S/g, function(a) { return a.toUpperCase(); });
}

console.log(capitalizeString("hei, dette er et eksempel!")); // Output: "Hei, Dette Er Et Eksempel!"
```

## Dypdykk
Kapitalisering av strenger er ikke en ny idé. Tilbake til skrivemaskinens dager, manuelle endringer ble gjort for viktige ord. I JavaScript, uten en innebygd funksjon som direkte kapitaliserer hver ord i en streng, må vi lage vår egen løsning. Det finnes alternativer til vår `capitalizeString` funksjon, for eksempel, biblioteker som Lodash har en `_.startCase` som gjør noe likt. Implementasjonen ovenfor bruker en regular expression som finner det første tegnet i et ord og en `.toUpperCase()` funksjon for å konvertere det til stor bokstav. Husk at funksjonen ikke skiller mellom ord, og vil også kapitalisere etter spesialtegn som bindestreker eller apostrofer.

## Se Også:
- MDN Web Docs String's `toUpperCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
- Lodash `_.startCase`: https://lodash.com/docs/4.17.15#startCase
- CSS tekst-transformasjon for kapitalisering: https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform
