---
title:                "Generierung von Zufallszahlen"
aliases: - /de/typescript/generating-random-numbers.md
date:                  2024-01-27T20:35:26.203969-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen in TypeScript dreht sich darum, unberechenbare numerische Werte innerhalb eines spezifizierten Bereichs zu erschaffen. Programmierer nutzen diese zufälligen Zahlen für eine Vielzahl von Zwecken, wie etwa das Erzeugen einzigartiger Kennungen, das Simulieren von Daten für Tests oder das Hinzufügen von Unvorhersehbarkeit zu Spielen und Simulationen.

## Wie:

In TypeScript kannst du mithilfe des globalen `Math`-Objekts Zufallszahlen generieren. Unten sind einige praktische Beispiele aufgeführt, die zeigen, wie man Zufallszahlen für unterschiedliche Anforderungen produziert.

### Eine grundlegende Zufallszahl generieren

Um eine grundlegende zufällige Dezimalzahl zwischen 0 (einschließlich) und 1 (ausschließlich) zu generieren, verwendest du `Math.random()`. Dies erfordert keine zusätzliche Manipulation:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Dies könnte einen Wert wie `0.8995452185604771` ausgeben.

### Eine zufällige Ganzzahl zwischen zwei Werten generieren

Wenn du eine Ganzzahl zwischen zwei spezifischen Werten benötigst, kombinierst du sowohl `Math.random()` als auch etwas Arithmetik:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Dies könnte einen Ganzzahlwert zwischen 1 und 10 ausgeben, wie zum Beispiel `7`.

### Eine einzigartige Kennung generieren

Zufallszahlen können mit anderen Methoden kombiniert werden, um einzigartige Kennungen zu erstellen, zum Beispiel ein einfaches UUID-Generator-Snippet:

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

Dies erzeugt eine Zeichenkette, die einer UUID ähnelt, wie zum Beispiel `110e8400-e29b-41d4-a716-446655440000`.

## Vertiefung

Die primäre Methode zur Generierung von Zufallszahlen in JavaScript und damit in TypeScript, `Math.random()`, stützt sich auf einen Pseudozufallszahlengenerator (PRNG). Es ist wichtig zu beachten, dass die Ergebnisse, obwohl sie zufällig erscheinen mögen, durch einen deterministischen Algorithmus basierend auf einem anfänglichen Seed-Wert erzeugt werden. Daher sind die durch `Math.random()` produzierten Zahlen nicht wirklich zufällig und sollten nicht für kryptographische Zwecke verwendet werden.

Für kryptografisch sichere Zufallszahlen bietet die Web Crypto API `crypto.getRandomValues()`, das in Umgebungen, die den Web Crypto-Standard unterstützen, einschließlich moderner Browser und Node.js (über das `crypto` Modul), zugänglich ist. Hier ist ein schnelles Beispiel, das dessen Verwendung in TypeScript zur Generierung einer sicheren Zufallszahl innerhalb eines Bereichs illustriert:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Diese Methode bietet eine stärkere Zufälligkeit und eignet sich eher für sicherheitssensible Anwendungen. Allerdings ist sie auch ressourcenintensiver und für einfache Aufgaben, wie einfache Simulationen oder die Erzeugung von nicht kritischen Zufallswerten, möglicherweise nicht notwendig.
