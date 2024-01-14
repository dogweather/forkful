---
title:    "TypeScript: Verketten von Zeichenfolgen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Zeichenfolgen ist eine grundlegende Operation in der Programmierung. Es ermöglicht die dynamische Erstellung von längeren Zeichenfolgen, die aus mehreren Teilen zusammengesetzt werden. Diese Technik ist besonders nützlich, wenn man beispielsweise Texte oder Nachrichten in einer Anwendung zusammenbauen möchte.

## Wie geht man vor?

Die Verkettung von Zeichenfolgen ist in TypeScript einfach und unkompliziert. Im Folgenden zeigen wir einige Beispiele und das resultierende Ergebnis.

```TypeScript
const name = "Max";
const age = 27;

const introduction = "Mein Name ist " + name + " und ich bin " + age + " Jahre alt.";
console.log(introduction);
```

Das obige Beispiel erstellt eine Zeichenfolge, die den Namen und das Alter einer fiktiven Person enthält. Die Variablen werden dabei durch das Pluszeichen aneinandergereiht und in der Konsole ausgegeben. Das Ergebnis lautet: "Mein Name ist Max und ich bin 27 Jahre alt."

Man kann auch mehrere Zeichenfolgen miteinander verketten, um komplexere Sätze zu konstruieren:

```TypeScript
const adjective = "intelligent";
const noun = "Engineer";
const profession = "Software";

const sentence = "Ich bin ein sehr " + adjective + " " + noun + " im Bereich der " + profession + "-Entwicklung.";
console.log(sentence);
```

Das Ergebnis ist: "Ich bin ein sehr intelligent Engineer im Bereich der Software-Entwicklung."

## Tiefergehende Informationen

In TypeScript können Zeichenfolgen auch mit dem Template Literal Operator (`) verketten werden. Dadurch können Variablen direkt in die Zeichenfolge eingefügt werden, ohne dass man jedes Mal das Pluszeichen verwenden muss:

```TypeScript
const pet = "Hund";

const greeting = `Hallo, mein Name ist Max. Ich habe einen ${pet}.`;
console.log(greeting);
```

Das Ergebnis ist: "Hallo, mein Name ist Max. Ich habe einen Hund."

Es ist auch möglich, JavaScript-Ausdrücke in den Platzhalter hinzuzufügen, um komplexe Verkettung von Zeichenfolgen zu ermöglichen:

```TypeScript
const x = 10;
const y = 5;

const result = `Die Summe von ${x} und ${y} ist ${x + y}.`;
console.log(result);
```

Das Ergebnis ist: "Die Summe von 10 und 5 ist 15."

## Siehe auch

- [Offizielle TypeScript-Website](https://www.typescriptlang.org/)
- [W3Schools TypeScript Tutorial](https://www.w3schools.com/typescript/)
- [Tutorial: String Concatenation in TypeScript](https://www.thecodehubs.com/type-of-string-concatenation-in-typescript/)