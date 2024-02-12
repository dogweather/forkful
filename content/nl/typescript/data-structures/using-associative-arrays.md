---
title:                "Gebruik van associatieve arrays"
aliases: - /nl/typescript/using-associative-arrays.md
date:                  2024-01-30T19:13:16.380287-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gebruik van associatieve arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays, of objecten in TypeScript, laten je strings (of sleutels) gebruiken om toegang te krijgen tot waardeparen. Programmeurs gebruiken ze voor dynamischere gegevenstoegangspatronen in vergelijking met traditionele arrays, en bieden een flexibele manier om data te structureren en toegang te krijgen zonder vast te zitten aan numerieke indexen.

## Hoe te:

Associatieve arrays maken en gebruiken in TypeScript is eenvoudig. Hier is een basishandleiding:

```TypeScript
// Een associatieve array verklaren
let user: { [key: string]: string } = {};

// Gegevens toevoegen
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Uitvoer:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Over sleutel-waardeparen itereren is ook gemakkelijk:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Uitvoer:

```TypeScript
name: Jane Doe
email: jane@example.com
```

En als je te maken hebt met een mix van gegevenstypen, komt het type systeem van TypeScript goed van pas:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Uitvoer:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## Diepgaand

In TypeScript zijn wat we associatieve arrays noemen in wezen objecten. Historisch gezien, in talen zoals PHP, zijn associatieve arrays een fundamenteel type, maar JavaScript (en bij uitbreiding, TypeScript) gebruikt objecten voor dit doel. Deze aanpak is zowel een kracht als een beperking. Objecten bieden een zeer dynamische structuur voor het koppelen van strings aan waarden, maar ze zijn niet bedoeld om te worden gebruikt als 'arrays' in de traditionele zin. Bijvoorbeeld, je kunt geen arraymethoden zoals `push` of `pop` direct op deze objecten gebruiken.

Voor gevallen waar je geordende collecties van sleutel-waardeparen met array-achtige operaties nodig hebt, biedt TypeScript (en modern JavaScript) het `Map` object:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Hoewel het type systeem van TypeScript en ES6-functies zoals `Map` krachtige alternatieven bieden, is het begrijpen van hoe objecten als associatieve arrays te gebruiken nuttig voor scenario's waar objectliteratuur efficiënter is of bij het werken met JSON-datastructuren. Het gaat erom het juiste gereedschap voor de klus te kiezen.
