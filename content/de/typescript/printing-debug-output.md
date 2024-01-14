---
title:                "TypeScript: Das Drucken von Debug-Ausgaben"
simple_title:         "Das Drucken von Debug-Ausgaben"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist eine wichtige Aktivität beim Entwickeln von Software. Es hilft uns dabei, Fehler in unserem Code zu finden und zu beheben. Eine Methode, die dabei helfen kann, ist das Drucken von Debug-Ausgaben im laufenden Programm. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie wir dies in TypeScript umsetzen können.

## Wie geht man vor?

Zunächst müssen wir sicherstellen, dass wir unser Programm im Debug-Modus ausführen. Dafür können wir entweder das entsprechende Flag beim Kompilieren setzen oder die debugging-funktion in unserer IDE aktivieren. Anschließend können wir in unserem Code Debug-Ausgaben über die `console.log()` Funktion erstellen.

Angular: 

```TypeScript
let name = "Lisa";
console.log("Hello " + name + ", welcome to the blog!");
```

Output:

```
Hello Lisa, welcome to the blog!
```

## Tiefergehende Informationen

Neben der `console.log()` Funktion gibt es in TypeScript auch die Möglichkeit, spezielle Debug-Ausgaben mittels Annotationen hinzuzufügen. Dies kann beispielsweise in Kombination mit dem `@ts-ignore` Befehl genutzt werden, um bestimmte Codebereiche auszuschließen oder zu untersuchen. Eine weitere Alternative ist das Einbinden eines Debuggers, der es ermöglicht, Schritt für Schritt durch unseren Code zu gehen und Variablenwerte zu überprüfen.

## Siehe auch

- [Typescript Debugging in Visual Studio Code](https://code.visualstudio.com/docs/nodejs/typescript-debugging)
- [Debugging in Angular Using Chrome DevTools](https://angular.io/guide/debugging)
- [Debugging in TypeScript with breakpoints](https://egghead.io/lessons/typescript-debugging-in-typescript-with-breakpoints)