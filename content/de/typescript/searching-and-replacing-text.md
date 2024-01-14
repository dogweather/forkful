---
title:    "TypeScript: Textsuche und Ersetzung"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Warum

Textsuche und -ersetzung ist ein grundlegender Bestandteil der Programmierung. Oft müssen wir bestimmte Begriffe oder Zeichenfolgen in unserem Code finden und ersetzen, um ihn effizienter und fehlerfreier zu machen. Dieser Artikel zeigt Ihnen, wie Sie mit TypeScript Textsuche und -ersetzung durchführen können.

## Wie geht das?

Um Textsuche und -ersetzung in TypeScript durchzuführen, verwenden wir die `replace()`-Methode. Diese Methode durchsucht eine Zeichenkette nach einem bestimmten Muster und ersetzt es durch eine neue Zeichenkette. Hier ist ein Beispiel, wie das in TypeScript aussehen könnte:

```TypeScript
let text = "Hallo Welt";
let newText = text.replace("Welt", "Leser");

console.log(newText); // Ausgabe: Hallo Leser
```

In diesem Beispiel haben wir die `replace()`-Methode verwendet, um das Wort "Welt" durch "Leser" zu ersetzen. Beachten Sie, dass die `replace()`-Methode nur das erste Vorkommen des Musters ersetzt. Um alle Vorkommen zu ersetzen, müssen Sie ein Reguläres Ausdrucksmuster verwenden.

Ein weiteres nützliches Werkzeug für Textsuche und -ersetzung ist der `indexOf()`-Methode. Diese Methode sucht nach einem Muster in einer Zeichenkette und gibt die erste Position zurück, an der es gefunden wurde. Wenn das Muster nicht gefunden wurde, gibt sie -1 zurück. Hier ist ein Beispiel:

```TypeScript
let text = "Dies ist ein Textbeispiel";
let position = text.indexOf("Text");

console.log(position); // Ausgabe: 8
```

In diesem Beispiel haben wir die `indexOf()`-Methode verwendet, um die Position des Wortes "Text" in der Zeichenkette zu finden.

## Tiefer Einblick

Um auch alle anderen Vorkommen eines Musters in einer Zeichenkette zu ersetzen, können wir ein Reguläres Ausdrucksmuster und den `replaceAll()`-Methode verwenden. Hier ist ein Beispiel:

```TypeScript
let text = "ABCABCABC";
let newText = text.replaceAll("ABC", "123");

console.log(newText); // Ausgabe: 123123123
```

Beachten Sie, dass in diesem Beispiel alle Vorkommen des Musters "ABC" durch "123" ersetzt wurden.

Es gibt auch weitere nützliche Methoden wie `toLowerCase()` und `toUpperCase()`, die Ihnen helfen können, den Suchprozess flexibler zu gestalten, indem Sie Groß- und Kleinschreibung ignorieren.

## Siehe auch

- [Dokumentation zu `replace()`-Methode von TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#regex-replace)
- [Dokumentation zu `indexOf()`-Methode von TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#string-search)
- [Dokumentation zu `replaceAll()`-Methode von TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#regex-replaceall)