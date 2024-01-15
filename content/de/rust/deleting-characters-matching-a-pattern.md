---
title:                "Entfernen von Zeichen, die einem Muster entsprechen"
html_title:           "Rust: Entfernen von Zeichen, die einem Muster entsprechen"
simple_title:         "Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Löschen von Zeichen in einem Text beschäftigen? Ganz einfach: Es kann die Effizienz und Lesbarkeit des Codes verbessern und unerwünschte Zeichen entfernen, die die Funktionalität beeinträchtigen könnten.

## Wie Funktioniert Es

Das Löschen von Zeichen anhand eines Musters kann in Rust auf verschiedene Arten erreicht werden. Eine Möglichkeit ist die Verwendung von regulären Ausdrücken, die in der Standardbibliothek von Rust verfügbar sind. Nehmen wir zum Beispiel an, dass wir alle Leerzeichen aus einem String entfernen wollen:

```Rust
let string = "Hallo, Welt!";
let new_string = string.replace(" ", "");
println!("{}", new_string); // Ausgabe: Hallo,Welt!
```

Wie du sehen kannst, haben wir die `replace`-Methode verwendet, um alle Leerzeichen aus dem String zu entfernen. Wir können auch ein reguläres Ausdrucksmuster als String übergeben, um alle Zeichen, die diesem Muster entsprechen, zu entfernen. Zum Beispiel:

```Rust
let string = "Dies~ist~ein~Beispiel~String~mit~Tilden~als~Trennzeichen~";
let new_string = string.replace("~", "");
println!("{}", new_string); // Ausgabe: DiesisteinBeispielStringmitTildenalsTrennzeichen
```

Einfach, oder? Aber was ist, wenn wir mehr Kontrolle über das Löschen von Zeichen haben wollen, zum Beispiel nur Zeichen an bestimmten Positionen oder in einem bestimmten Bereich des Strings löschen? Hier kommt die `chars()`-Methode ins Spiel, die uns eine Iterator über jeden einzelnen Buchstaben im String gibt. Wir können dann entscheiden, welche Zeichen wir behalten und welche wir löschen wollen. Schauen wir uns ein Beispiel an:

```Rust
let string = "123abc456def";
let new_string: String = string.chars() // Erhalte einen Iterator über jeden Buchstaben im String
    .enumerate() // Nummeriere jeden Buchstaben, um die Position zu erhalten
    .filter(|&(index, _)| index % 3 != 0) // Entferne jeden dritten Buchstaben
    .map(|(_, letter)| letter) // Entferne die Nummerierung und behalte nur die Buchstaben
    .collect(); // Sammle die übrigen Buchstaben wieder zu einem String

println!("{}", new_string); // Ausgabe: 12457ef
```

Interessant, oder? Indem wir die `enumerate()`- und `filter()`-Methode kombinieren, können wir die Position der einzelnen Buchstaben herausfinden und basierend auf dieser Position entscheiden, ob wir den Buchstaben behalten oder entfernen wollen. Dann verwenden wir `map()` und `collect()` um den Iterator wieder zu einem String zu sammeln.

## Tiefer Einblick

Es gibt noch viele weitere Möglichkeiten, um Zeichen in Rust zu löschen, zum Beispiel durch die Verwendung von Slices oder dem crate `unicode_segmentation`. Wenn du tiefer in die Materie einsteigen möchtest, empfehlen wir dir, die offizielle Rust-Dokumentation zu lesen und verschiedene Methoden auszuprobieren.

## Siehe Auch

- [Offizielle Rust-Dokumentation](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Crate: Unicode_segmentation](https://crates.io/crates/unicode_segmentation)