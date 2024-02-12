---
title:                "Een string omzetten naar kleine letters"
aliases: - /nl/javascript/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:58:07.543270-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een string converteren naar kleine letters betekent het transformeren van alle tekens binnenin naar hun kleine letter varianten. Programmeurs doen dit voor consistentie, vooral voor hoofdletterongevoelige vergelijkingen, zoals bij het normaliseren van gebruikersinvoer of het zoeken in tekstinhoude.

## Hoe te:
In JavaScript converteren we een string naar kleine letters met de methode `.toLowerCase()`. Het is zo simpel als:

```javascript
let groet = "Hallo, Wereld!";
let kleineLetterGroet = groet.toLowerCase();
console.log(kleineLetterGroet); // "hallo, wereld!"
```

Wanneer gebruikt, wordt elk karakter in de originele string, indien mogelijk, omgezet naar kleine letters:

```javascript
let gemengdeHoofdletters = "jAvAScript ROCKs!";
let naarKleineLetters = gemengdeHoofdletters.toLowerCase();
console.log(naarKleineLetters); // "javascript rocks!"
```

Merk op dat tekens die geen kleine letter equivalenten hebben onveranderd blijven.

## Diepgaande Duik
In de oude dagen betekende het omgaan met tekst oppassen voor karaktercoderingen en handmatige conversies. Maar in moderne JavaScript abstraheert `.toLowerCase()` die complexiteiten weg. Onderliggend gebruikt het Unicode-mapping om karakters te converteren, dus het werkt met meer dan alleen A-Z.

Er bestaan alternatieve methoden, zoals:

- `toLocaleLowerCase()`: Dit houdt rekening met de lokale instelling van de gebruiker, wat essentieel is voor bepaalde talen waar de regels voor kleine letters contextspecifiek zijn.

- Reguliere expressies: Voordat `toLowerCase()` bestond, zouden ontwikkelaars regex kunnen hebben gebruikt om hoofdletters handmatig te vervangen.

Detailsgewijs, onthoud dat `.toLowerCase()` de originele string niet verandert (strings in JavaScript zijn onveranderlijk). Je krijgt altijd een nieuwe string. Het behandelt ook alle karakters die door de Unicode-standaard als hoofdletter worden herkend, wat betekent dat je gedekt bent over verschillende talen en scripts.

## Zie Ook
- [MDN webdocumentatie over toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Unicode-standaard voor casing](https://unicode.org/reports/tr21/tr21-5.html)
- [Hoofdletters en kleine letters met Locale: toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
