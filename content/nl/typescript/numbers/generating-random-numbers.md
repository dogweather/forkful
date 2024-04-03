---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:12.536188-07:00
description: "Het genereren van willekeurige getallen in TypeScript gaat over het\
  \ cre\xEBren van onvoorspelbare numerieke waarden binnen een gespecificeerd bereik.\u2026"
lastmod: '2024-03-13T22:44:50.546180-06:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in TypeScript gaat over het cre\xEB\
  ren van onvoorspelbare numerieke waarden binnen een gespecificeerd bereik."
title: Willekeurige getallen genereren
weight: 12
---

## Wat & Waarom?

Het genereren van willekeurige getallen in TypeScript gaat over het creëren van onvoorspelbare numerieke waarden binnen een gespecificeerd bereik. Programmeurs maken gebruik van deze willekeurige cijfers voor verschillende doeleinden, zoals het genereren van unieke identificeerders, het simuleren van gegevens voor testen, of het toevoegen van onvoorspelbaarheid aan spellen en simulaties.

## Hoe te:

In TypeScript kunt u willekeurige getallen genereren met behulp van het globale `Math`-object. Hieronder staan enkele praktische voorbeelden die demonstreren hoe willekeurige getallen voor verschillende behoeften geproduceerd kunnen worden.

### Een Basis Willekeurig Getal Genereren

Om een basis willekeurig decimaal getal tussen 0 (inclusief) en 1 (exclusief) te genereren, gebruikt u `Math.random()`. Dit vereist geen aanvullende manipulatie:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Dit kan een waarde uitvoeren zoals `0.8995452185604771`.

### Een Willekeurig Geheel Getal Genereren Tussen Twee Waarden

Wanneer u een geheel getal nodig heeft tussen twee specifieke waarden, gebruikt u zowel `Math.random()` als wat rekenwerk:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Dit kan een geheel getal waarde uitvoeren tussen 1 en 10, zoals `7`.

### Een Unieke Identificatiecode Genereren

Willekeurige getallen kunnen worden gecombineerd met andere methoden om unieke identificatiecodes te creëren, bijvoorbeeld een eenvoudig UUID-generatorfragment:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

Dit genereert een string die lijkt op een UUID, zoals `110e8400-e29b-41d4-a716-446655440000`.

## Diepgaande Duik

De primaire methode voor het genereren van willekeurige getallen in JavaScript en dus in TypeScript, `Math.random()`, is afhankelijk van een pseudo-willekeurige getallengenerator (PRNG). Het is belangrijk om te noteren dat hoewel de resultaten willekeurig kunnen lijken, ze worden gegenereerd door een deterministisch algoritme gebaseerd op een initiële zaadwaarde. Daarom zijn de door `Math.random()` geproduceerde getallen niet echt willekeurig en mogen ze niet worden gebruikt voor cryptografische doeleinden.

Voor cryptografisch veilige willekeurige getallen biedt de Web Crypto API `crypto.getRandomValues()`, die toegankelijk is in omgevingen die de Web Crypto-standaard ondersteunen, inclusief moderne browsers en Node.js (via de `crypto`-module). Hier is een snel voorbeeld dat het gebruik in TypeScript illustreert voor het genereren van een veilig willekeurig getal binnen een bereik:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Deze methode biedt een sterker niveau van willekeurigheid en is beter geschikt voor beveiligingsgevoelige toepassingen. Het is echter ook meer bronnenintensief en is mogelijk niet nodig voor meer alledaagse taken, zoals eenvoudige simulaties of niet-kritieke willekeurige waardegeneratie.
