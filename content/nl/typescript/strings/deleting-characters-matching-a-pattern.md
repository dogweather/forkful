---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:03.721702-07:00
description: "Het verwijderen van tekens die overeenkomen met een patroon houdt in\
  \ dat je in een string zoekt naar een specifieke reeks tekens (het patroon) en deze\u2026"
lastmod: '2024-03-13T22:44:50.533443-06:00'
model: gpt-4-0125-preview
summary: Het verwijderen van tekens die overeenkomen met een patroon houdt in dat
  je in een string zoekt naar een specifieke reeks tekens (het patroon) en deze verwijdert.
title: Karakters verwijderen die overeenkomen met een patroon
weight: 5
---

## Hoe te:
```TypeScript
function deletePattern(text: string, pattern: string): string {
  // Maak een RegExp van de patroonreeks
  const regex = new RegExp(pattern, 'g');
  // Vervang voorkomens van het patroon door een lege string
  return text.replace(regex, '');
}

// Voorbeeldgebruik
const originalText = "Hallo, Wereld! Dit -- is een test.";
const newText = deletePattern(originalText, "[,\\-!]");
console.log(newText);  // Output: "Hallo Wereld Dit  is een test"
```

## Diepgaande verkenning
Historisch gezien kan het omgaan met strings in programmering worden herleid tot de dageraad van de informatica. In TypeScript, dat voortbouwt op JavaScript, is het manipuleren van strings een dagelijkse taak. De functie `replace()` die we hebben gebruikt, is geërfd van JavaScript's robuuste arsenaal voor stringmanipulatie.

Er zijn alternatieven voor RegExp om patronen te matchen - soms wil je misschien handmatig door elk teken itereren en beslissingen nemen met een switch-statement of een reeks ifs. Maar reguliere expressies bieden een beknopte en krachtige manier om complexe patronen te beschrijven voor matching.

Implementatiedetails worden interessant wanneer je dieper ingaat op hoe RegExp-patronen worden geïnterpreteerd tijdens runtime. De 'g'-vlag in de RegExp-constructor geeft de engine de opdracht om globaal door de string te zoeken. Zonder dit zou alleen de eerste match worden vervangen. Reguliere expressies kunnen eenvoudig of onbegrijpelijk complex zijn, afhankelijk van je behoeften.

## Zie ook
- De MDN Web Docs over RegExp: [https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/RegExp](https://developer.mozilla.org/nl/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- TypeScript Handboek over stringmanipulatie: [https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- Reguliere expressies tester om te helpen bij het creëren van patronen: [https://regexr.com/](https://regexr.com/)
