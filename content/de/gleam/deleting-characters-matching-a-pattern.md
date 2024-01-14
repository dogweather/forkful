---
title:    "Gleam: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann ein nützliches Werkzeug beim Programmieren sein, um unerwünschte oder überflüssige Daten zu entfernen. Es kann auch dazu beitragen, den Code übersichtlicher und besser lesbar zu machen. In diesem Blog-Beitrag zeigen wir Ihnen, wie Sie dies mit der Programmiersprache Gleam erreichen können.

## So geht's

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können Sie die Funktion `String.replace` verwenden. Diese Funktion nimmt zwei Argumente an, das zu durchsuchende String und das Muster, das Sie finden und ersetzen möchten. Wenn Sie nur Zeichen löschen möchten, können Sie das zweite Argument, das den Ersatzstring enthält, einfach leer lassen.

```Gleam
my_string = "Hello World!"
new_string = String.replace(my_string, "o", "")
```

Das obige Beispiel würde alle Vorkommen des Buchstabens "o" in dem String "Hello World!" löschen und den Ergebnisstring "Hell Wrld!" zurückgeben.

Es ist auch möglich, mehrere Muster gleichzeitig zu löschen, indem Sie das zweite Argument als Liste anstelle einer einzelnen Zeichenkette übergeben. Zum Beispiel können Sie alle Vorkommen von "e" und "o" in einem String mit folgendem Code entfernen:

```Gleam
my_string = "Hello World!"
new_string = String.replace(my_string, ["e", "o"], "")
```

Das Ergebnis wäre dann "Hll Wrld!".

## Tiefere Einblicke

Wenn Sie mehr über die `String.replace`-Funktion erfahren möchten, können Sie in der offiziellen Gleam-Dokumentation nachlesen. Dort finden Sie weitere Informationen über die verschiedenen Möglichkeiten, die diese Funktion bietet, und Beispiele für deren Verwendung.

Sie sollten auch beachten, dass die `String.replace`-Funktion immer eine neue Zeichenkette zurückgibt und den ursprünglichen String unverändert lässt. Wenn Sie also sicherstellen möchten, dass die Änderungen am neu erstellten String übernommen werden, müssen Sie diesen in einer neuen Variablen speichern oder den ursprünglichen String durch den neuen ersetzen.

## Siehe auch

- [Gleam-Dokumentation zur `String.replace`-Funktion](https://gleam.run/documentation/#string.replace)
- [Offizielle Gleam-Website](https://gleam.run/)
- [Gleam-Community auf Reddit](https://www.reddit.com/r/gleamlang/)