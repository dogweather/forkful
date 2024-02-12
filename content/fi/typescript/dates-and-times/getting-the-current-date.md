---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- /fi/typescript/getting-the-current-date.md
date:                  2024-02-03T19:11:05.156265-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Nykyisen päivämäärän hankkiminen TypeScriptissä, joka on JavaScript-pohjainen kieli, mahdollistaa nykyisen päivämäärän ja ajan tiedon käyttämisen ja manipuloinnin. Ohjelmoijat tarvitsevat usein tätä toiminnallisuutta luodakseen aikaleimoja, aikataulutusta ja muita aikaan sidottuja piirteitä sovelluksiinsa.

## Kuinka:
TypeScriptissä voit käyttää `Date`-objektia nykyisen päivämäärän ja ajan saamiseksi. Näin voit tehdä sen:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Esimerkkitulostus:
```
2023-04-12T07:20:50.52Z
```

Tämä koodipätkä luo uuden `Date`-objektin, joka sisältää nykyisen päivämäärän ja ajan, ja tulostaa sen konsoliin. Voit myös muotoilla päivämäärän käyttämällä toLocaleDateString()-metodia saadaksesi luettavampia muotoja:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Esimerkkitulostus:
```
4/12/2023
```

### date-fns:n käyttö
Laajempaan päivämäärien manipulointiin ja muotoiluun `date-fns`-kirjasto on suosittu valinta. Asenna se ensin npm:n kautta:

```bash
npm install date-fns
```

Sen jälkeen voit käyttää sitä nykyisen päivämäärän muotoiluun:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Esimerkkitulostus:
```
2023-04-12
```

Tämä `date-fns`-esimerkki muotoilee nykyisen päivämäärän merkkijonoksi "YYYY-MM-DD" -muodossa. Kirjasto tarjoaa lukuisia toimintoja päivämäärien manipulointiin, tehden siitä monipuolisen työkalun kaikille TypeScript-ohjelmoijille, jotka työskentelevät päivämäärien parissa.
