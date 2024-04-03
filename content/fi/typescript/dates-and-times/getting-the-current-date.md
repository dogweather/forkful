---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:05.156265-07:00
description: "Kuinka: TypeScriptiss\xE4 voit k\xE4ytt\xE4\xE4 `Date`-objektia nykyisen\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan saamiseksi. N\xE4in voit tehd\xE4 sen."
lastmod: '2024-03-13T22:44:56.325607-06:00'
model: gpt-4-0125-preview
summary: "TypeScriptiss\xE4 voit k\xE4ytt\xE4\xE4 `Date`-objektia nykyisen p\xE4iv\xE4\
  m\xE4\xE4r\xE4n ja ajan saamiseksi."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

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
