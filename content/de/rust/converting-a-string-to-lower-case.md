---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Rust: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was ist das und warum?

Wenn wir über die Umwandlung von Strings in Kleinbuchstaben sprechen, meinen wir die Veränderung der Buchstaben in einem Text von Großbuchstaben zu entsprechenden Kleinbuchstaben. Programmierer tun dies, um sicherzustellen, dass die Texteingabe einheitlich ist und dass sie leichter auf bestimmte Schlüsselwörter überprüft werden kann, ohne sich um die Groß- und Kleinschreibung kümmern zu müssen.

## Wie geht's?

```Rust
let str = "Hallo WELT!";
let new_str = str.to_lowercase();
println!("{}", new_str);
```

Das obige Beispiel zeigt die Verwendung der Funktion "to_lowercase()", die in Rust bereits eingebaut ist. Sie nimmt einen String als Eingabe und gibt einen neuen String mit allen Buchstaben in Kleinbuchstaben zurück. In diesem Fall würde die Ausgabe "hallo welt!" lauten.

## Tiefere Einblicke

Die Umwandlung von Strings in Kleinbuchstaben ist eine gängige Praxis in der Programmierung, um sicherzustellen, dass Benutzereingaben standardisiert und einfacher zu vergleichen sind. In der Vergangenheit mussten Programmierer oft eigene Funktionen schreiben, um diese Umwandlung durchzuführen, aber dank der eingebauten Funktion in Rust wird es nun einfacher und effizienter. Es gibt auch alternative Methoden, wie z.B. die Verwendung von regulären Ausdrücken, um spezifischere Anpassungen an der Groß- und Kleinschreibung durchzuführen.

## Weiterführende Links

- [offizielle Rust-Dokumentation zu Strings](https://doc.rust-lang.org/std/string/struct.String.html)
- [Tutorial zu regulären Ausdrücken in Rust](https://doc.rust-lang.org/1.5.0/book/regex.html)