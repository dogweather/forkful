---
title:                "Umformung eines Strings in Kleinbuchstaben"
aliases:
- de/rust/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:04.357519-07:00
model:                 gpt-4-1106-preview
simple_title:         "Umformung eines Strings in Kleinbuchstaben"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
String in Kleinbuchstaben umwandeln bedeutet, alle Großbuchstaben in einem String in ihre entsprechenden Kleinbuchstaben zu konvertieren. Wir machen das für konsistente Datenverarbeitung und Vergleiche, zum Beispiel bei der Eingabeprüfung.

## Anleitung:
Um in Rust einen String in Kleinbuchstaben umzuwandeln, benutze die Methode `to_lowercase()`. Hier ist ein einfaches Beispiel:

```rust
fn main() {
    let original = "Rust Programmierung!";
    let kleingeschrieben = original.to_lowercase();

    println!("Original: {}", original);
    println!("Kleingeschrieben: {}", kleingeschrieben);
}
```

Ausgabe:

```
Original: Rust Programmierung!
Kleingeschrieben: rust programmierung!
```

## Vertiefung:
Die `to_lowercase()` Funktion in Rust ist mehr als nur eine Schleife, die durch jeden Buchstaben läuft und ihn umwandelt – sie kümmert sich um internationale Schriftzeichen und befolgt die Unicode-Standards. Alternativen umfassen die Nutzung von `as_bytes()` für ASCII-Zeichen oder eigene Implementierung mit Kontrolle über den Umgang mit Sonderfällen – nicht jedoch empfohlen. Historischer Kontext: Die Beachtung von Unicode-Normen hat gerade in einer globalisierten Welt an Bedeutung gewonnen, daher ist die Standardimplementierung sorgfältig darauf ausgelegt.

## Siehe Auch:
- Rust Dokumentation für `to_lowercase()`: https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase
- Unicode-Konsortium, um zu verstehen, wie internationale Schriftzeichen standardisiert werden: http://unicode.org/
- Vergleiche zwischen Groß- und Kleinschreibung in der Programmierung: https://en.wikipedia.org/wiki/Case_sensitivity
