---
title:                "Sette stor bokstav i en streng"
aliases:
- /no/typescript/capitalizing-a-string/
date:                  2024-02-03T19:06:43.846755-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sette stor bokstav i en streng"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sette stor forbokstav i en streng innebærer å endre det første tegnet av en gitt streng til stor bokstav hvis den er i små bokstaver, ofte uten å endre resten av strengen. Denne operasjonen brukes typisk for å sikre at egennavn eller begynnelsen av setninger følger grammatikalske regler i tekstbehandling, noe som gjør output ser profesjonell og lesbar ut.

## Hvordan:

TypeScript, som er et superset av JavaScript, tillater forskjellige metoder for å sette stor forbokstav i strenger, som spenner fra rene JavaScript-tilnærminger til å benytte tredjepartsbiblioteker for mer komplekse eller spesifikke brukstilfeller.

**Ren JavaScript-tilnærming:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Eksempel på output:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Denne metoden er grei og avhenger av `charAt()`-metoden for å få tilgang til det første tegnet i strengen og `toUpperCase()` for å konvertere det til stor bokstav. `slice(1)`-metoden henter deretter resten av strengen, og lar den være uendret.

**Bruke Lodash-biblioteket:**

For prosjekter som allerede bruker [Lodash](https://lodash.com/)-biblioteket, kan du bruke dens `_.capitalize`-funksjon for å oppnå samme resultat med mindre kode.

Først, installer Lodash:

```bash
npm install lodash
```

Deretter, bruk det i din TypeScript-fil:

```typescript
import * as _ from 'lodash';

// Eksempel på output:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Merk: Lodash sin `_.capitalize`-metode konverterer resten av strengen til små bokstaver, noe som kanskje ikke alltid er det du ønsker.

**Bruke et regulært uttrykk:**

Et regulært uttrykk kan gi en kortfattet måte å sette stor forbokstav på det første bokstaven i en streng, spesielt hvis du trenger å sette stor forbokstav på den første bokstaven i hvert ord i en streng.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Eksempel på output:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Denne metoden bruker `replace()`-funksjonen for å søke etter en ordgrense etterfulgt av et alfanumerisk tegn (`\b\w`), og setter stor bokstav på hvert funn. Den er spesielt hendig for titler eller overskrifter.
