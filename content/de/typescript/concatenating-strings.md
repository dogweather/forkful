---
title:    "TypeScript: Zusammenfügen von Zeichenketten"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Warum
Die Verkettung von Strings ist eine häufige Aufgabe in der Programmierung. Durch das Kombinieren (Verkettung) von mehreren Strings können wir komplexe Ausgaben erstellen oder Texte dynamisch generieren. In diesem Blog-Beitrag werden wir uns ansehen, wie wir in TypeScript Strings verketteten können.

# Wie geht man vor
Zuerst müssen wir verstehen, wie Strings in TypeScript funktionieren. In TypeScript bezieht sich der String-Typ auf eine Zeichenfolge. Wir können Strings mit doppelten Anführungszeichen (" ") oder einfachen Anführungszeichen (' ') definieren.

Beispiel:
```TypeScript
let name: string = "Max";
let greeting: string = "Hallo";
```

Um Strings zu verketteten, können wir den + Operator verwenden. Hierbei werden die Strings einfach aneinandergehängt.

Beispiel:
```TypeScript
let message: string = greeting + " " + name;
console.log(message); // Ausgabe: Hallo Max
```

In TypeScript können wir auch Template-Strings verwenden, um Strings zu verketteten. Diese werden mit Backticks (` `) anstatt von Anführungszeichen definiert. Wir können dann Platzhalter innerhalb des Strings verwenden, indem wir diese mit ${ } umschließen.

Beispiel:
```TypeScript
let message: string = `${greeting} ${name}`;
console.log(message); // Ausgabe: Hallo Max
```

# Tieferer Einblick
Bei der Verkettung von Strings müssen wir beachten, dass es möglicherweise eine bessere Lösung gibt, wenn wir längere Texte oder dynamische Inhalte erstellen wollen. Hierfür können wir die String-Interpolation-Funktion nutzen.

Beispiel:
```TypeScript
let num: number = 10;
let calculation: string = `${num} * ${num} = ${num * num}`;
console.log(calculation); // Ausgabe: 10 * 10 = 100
```

Es ist auch wichtig anzumerken, dass bei der Verkettung von mehreren Strings mit dem + Operator, jedes Mal wenn wir eine neue String-Kombination erstellen, es zu einer erneuten Allokation des Speichers kommt. Wenn wir also viele Strings verketteten, kann dies zu Performance-Problemen führen. In solchen Fällen ist es sinnvoller, einen StringBuilder zu verwenden.

# Siehe auch
- https://www.typescriptlang.org/docs/handbook/basic-types.html#string
- https://www.typescriptlang.org/docs/handbook/using-types-as-classes.html#string-interpolation
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals