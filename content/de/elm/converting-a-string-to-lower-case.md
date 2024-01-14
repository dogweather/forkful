---
title:    "Elm: Umwandeln eines Strings in Kleinbuchstaben."
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man möglicherweise eine Zeichenkette in Kleinbuchstaben umwandeln möchte: Es kann zum Beispiel sein, dass man sicherstellen möchte, dass die Eingabe des Benutzers immer einheitlich verarbeitet wird, oder dass man Vergleiche zwischen verschiedenen Zeichenketten durchführt, die in verschiedenen Schreibweisen vorliegen können.

## Wie geht man vor?

Die Umwandlung einer Zeichenkette in Kleinbuchstaben ist in Elm einfach und intuitiv. Dafür gibt es eine Funktion namens `String.toLower`, die genau das erledigt, was der Name sagt. Man ruft diese Funktion einfach auf und übergibt ihr die zu konvertierende Zeichenkette als Argument. Hier ein Beispiel:

```Elm
String.toLower "ELM PROGRAMMIEREN"
```

Das Ergebnis dieser Funktion ist die ausgegebene Zeichenkette "elm programmieren". Wie man sehen kann, werden alle Großbuchstaben in Kleinbuchstaben umgewandelt. Wenn die Eingabe bereits aus Kleinbuchstaben besteht, bleibt diese unverändert. Hier ein weiteres Beispiel:

```Elm
String.toLower "EiNfAcH eLm PrOGraMMierEn"
```

Das Ergebnis hier wäre "einfach elm programmieren", da alle Buchstaben in Kleinbuchstaben umgewandelt werden.

## Tiefere Einblicke

Um zu verstehen, wie die Funktion `String.toLower` funktioniert, lohnt es sich, einen kurzen Blick auf die zugrundeliegende Implementierung zu werfen. Grundsätzlich wandelt diese Funktion jeden einzelnen Buchstaben in der Zeichenkette in Kleinbuchstaben um. Hierfür verwendet Elm die interne Funktion `String.map` und die `Char.toLower` Funktion. Diese beiden Funktionen sind Teil der Standardbibliothek von Elm.

Eine weitere interessante Tatsache ist, dass es auch eine Funktion `String.toUpper` gibt, die die Zeichenkette in Großbuchstaben umwandelt. Hier ein Beispiel:

```Elm
String.toUpper "elm programmieren"
```

Das Ergebnis wäre "ELM PROGRAMMIEREN". Wie man sieht, funktioniert diese Funktion auf die gleiche Weise wie `String.toLower`, nur dass hier natürlich die `Char.toUpper` Funktion verwendet wird.

## Siehe auch

- [Offizielle Elm Dokumentation für `String.toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Offizielle Elm Dokumentation für `Char.toLower`](https://package.elm-lang.org/packages/elm/core/latest/Char#toLower)
- [Offizielle Elm Dokumentation für `String.toUpper`](https://package.elm-lang.org/packages/elm/core/latest/String#toUpper)
- [Offizielle Elm Dokumentation für `Char.toUpper`](https://package.elm-lang.org/packages/elm/core/latest/Char#toUpper)