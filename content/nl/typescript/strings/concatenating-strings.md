---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:05.282795-07:00
description: "Het samenvoegen van strings is het aan elkaar plakken van twee of meer\
  \ strings om \xE9\xE9n geheel te vormen. Programmeurs doen dit om berichten samen\
  \ te\u2026"
lastmod: '2024-03-13T22:44:50.541918-06:00'
model: gpt-4-0125-preview
summary: "Het samenvoegen van strings is het aan elkaar plakken van twee of meer strings\
  \ om \xE9\xE9n geheel te vormen."
title: Samenvoegen van strings
weight: 3
---

## Hoe doe je dat:
``` TypeScript
let groet: string = "Hallo";
let doelwit: string = "Wereld";
let bericht: string = groet + ", " + doelwit + "!"; // met behulp van de + operator
console.log(bericht); // Output: Hallo, Wereld!

let anderBericht: string = `${groet}, ${doelwit}!`; // gebruikmakend van template literals
console.log(anderBericht); // Output: Hallo, Wereld!
```

## Dieper in duiken
Samenvoeging is fundamenteel; het bestaat al sinds de vroege dagen van het programmeren. In TypeScript, dat voortbouwt op JavaScript, zijn we ver gekomen van de houterige stringbewerkingen naar gestroomlijnde template literals.

Historisch gezien moest je met samenvoeging voorzichtig zijn om niet te veel geheugen te gebruiken of de browser te vertragen. Moderne engines zijn geoptimaliseerd, maar efficiëntie is nog steeds belangrijk in grootschalige apps.

Er zijn alternatieven:
1. Arrays en `.join()`: Nuttig wanneer je te maken hebt met een lijst van strings.
2. StringBuilder-patronen: Meer relevant voor talen zoals Java of C# waar het de prestaties optimaliseert.

Wat implementatie betreft, compileert TypeScript uiteindelijk naar JavaScript. Onder de motorkap gebruikt het dezelfde stringfuncties en -bewerkingen die door JavaScript worden aangeboden.

## Zie ook
- Je wilt misschien de Mozilla Developer Network [String documentatie](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) bekijken voor een diepgaande blik op stringmethodes.
- Voor TypeScript-specifieke stringvragen is de [officiële documentatie van TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string) een snelle referentie.
