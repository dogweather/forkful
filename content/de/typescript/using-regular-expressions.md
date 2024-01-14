---
title:    "TypeScript: Verwendung von regulären Ausdrücken"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Regular Expressions sind ein wichtiges und leistungsstarkes Werkzeug in der TypeScript-Programmierung. Sie ermöglichen es uns, komplexe Muster in Zeichenketten zu finden und zu manipulieren. Dies ist besonders in der Verarbeitung von großen Datensätzen oder beim Validieren von Benutzereingaben nützlich.

## Wie man Regular Expressions in TypeScript verwendet

Um mit Regular Expressions in TypeScript zu arbeiten, müssen wir zunächst ein neues RegExp-Objekt erstellen. Dieses Objekt akzeptiert zwei Parameter: das zu suchende Muster und optionale Flags, die das Verhalten der Suche beeinflussen. Ein Beispiel hierfür könnte wie folgt aussehen:

```TypeScript
let re = new RegExp("[0-9]+", "gi");
```

In diesem Beispiel suchen wir nach einer beliebigen Anzahl von Ziffern in einem String und die Flags "g" (global) und "i" (case-insensitive) sorgen dafür, dass alle passenden Muster gefunden werden.

Um zu überprüfen, ob ein String einem bestimmten Muster entspricht, können wir die `test()`-Methode des RegExp-Objekts verwenden:

```TypeScript
if (re.test("123")) {
  console.log("Entspricht dem Muster!");
} else {
  console.log("Kein Match gefunden.");
}

// Ausgabe: "Entspricht dem Muster!"
```

Um tatsächlich die passenden Muster aus einem String zu extrahieren, können wir die `exec()`-Methode verwenden, die ein Array mit allen gefundenen Treffern zurückgibt. Hier ein Beispiel:

```TypeScript
let matches = re.exec("123abc456");
console.log(matches);

// Ausgabe: ["123", "456"]
```

Es ist auch möglich, mit sogenannten Capture Groups zu arbeiten, um bestimmte Teile des Musters in der Ausgabe zu definieren. Hierfür verwenden wir runde Klammern in unserem Muster und können dann mit `matches[1]` auf den Inhalt der ersten Capture Group zugreifen.

## Tiefere Einblicke in Regular Expressions

Es gibt viele nützliche Features und Tricks, die wir in unseren Regular Expressions nutzen können. Hier ein paar Beispiele:

- `|`: Mit dem Vertikalstrich können wir ein ODER-Verhältnis in unserem Muster definieren. Zum Beispiel: `/(Hund|Katze)/` würde nach "Hund" oder "Katze" suchen.
- `+` und `*`: Mit diesen Metazeichen können wir definieren, dass das vorhergehende Zeichen ein oder mehrmals (mit `+`) oder kein oder mehrmals (mit `*`) vorkommen muss.
- `\d`, `\w` und `\s`: Diese Abkürzungen stehen für Ziffern (`\d`), Alphanumerische Zeichen (`\w`) und Whitespace (`\s`).
- `^` und `$`: Diese Metazeichen stehen für den Anfang (`^`) bzw. das Ende (`$`) eines Strings und können verwendet werden, um zu überprüfen, ob ein String komplett dem Muster entspricht.

Es gibt noch viele weitere nützliche Funktionen und Möglichkeiten, die hier nicht alle aufgeführt werden können. Wir empfehlen, sich in die Thematik weiter einzulesen und die Möglichkeiten von Regular Expressions in TypeScript zu erforschen.

## Siehe auch

- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript: RegExp](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [RegExr: Online Regular Expression Tester](https://regexr.com/)