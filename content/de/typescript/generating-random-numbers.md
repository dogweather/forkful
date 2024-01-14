---
title:    "TypeScript: Zufallszahlen generieren"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist eine grundlegende Fähigkeit, die in der Programmierung häufig benötigt wird. Mit TypeScript können wir einfach und schnell Zufallszahlen erstellen, die in verschiedenen Szenarien nützlich sein können.

## Wie man es macht

Um in TypeScript Zufallszahlen zu generieren, müssen wir zunächst die `Math.random()` -Methode verwenden. Diese Methode gibt eine zufällige Zahl zwischen 0 (inklusive) und 1 (exklusive) zurück. Wir können dann diese Zahl mit einer einfachen Formel skalieren, um einen gewünschten Zahlenbereich zu erhalten.

```TypeScript
// Eine Zufallszahl zwischen 1 und 10 generieren
let randomNumber = Math.random() * 10 + 1; 
console.log(randomNumber); // Beispiel Ausgabe: 7.874553028327906
```

Wir können auch die `Math.floor()` -Methode verwenden, um die Nachkommastellen abzuschneiden und eine ganze Zufallszahl zu erhalten.

```TypeScript
// Eine ganze Zufallszahl zwischen 1 und 10 generieren
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // Beispiel Ausgabe: 7
```

Darüber hinaus kann die `Math.random()` -Methode als Grundlage für komplexere Zufallszahlengeneratoren verwendet werden, die spezifischen Anforderungen entsprechen.

## Tiefere Einblicke

Bei der Verwendung von `Math.random()` in TypeScript ist es wichtig, sich bewusst zu sein, dass die generierten Zahlen nicht wirklich zufällig sind. Sie folgen einem Algorithmus und können reproduziert werden, wenn das gleiche Seed (oder Saatgut) verwendet wird. Ein Seed kann als Startpunkt für den Algorithmus betrachtet werden und bestimmt, welche Zahlen generiert werden. Standardmäßig wird bei der Verwendung von `Math.random()` der aktuelle Zeitstempel als Seed verwendet.

Sie können auch einen benutzerdefinierten Seed verwenden, indem Sie eine Zahl in die `Math.random()` -Methode einfügen.

```TypeScript
// Verwendung eines benutzerdefinierten Seeds
Math.random(seed); 
```

Es ist auch möglich, einen Seed während der Entwicklung zu setzen, um konsistente Ergebnisse zu erhalten, indem Sie den Seed manuell ändern oder einen vordefinierten Seed verwenden.

## Siehe auch

- [Offizielle TypeScript Dokumentation zu Math.random()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#typed-math-random)
- [MDN Dokumentation zu Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Blog-Post: "Wie man Zufallszahlen in TypeScript generiert"](https://blog.logrocket.com/generating-random-numbers-in-typescript/)