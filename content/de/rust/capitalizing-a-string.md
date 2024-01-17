---
title:                "String großschreiben"
html_title:           "Rust: String großschreiben"
simple_title:         "String großschreiben"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Was ist das und warum?

Das Hochsetzen eines Strings bedeutet, alle Buchstaben in einem String so zu ändern, dass sie großgeschrieben sind. Programmierer machen das in der Regel, um eine konsistente Formatierung von Text zu gewährleisten.

Wie geht's?

Rust hat eine eingebaute Methode, um eine Zeichenkette zu kapitalisieren. Dies kann entweder durch die Verwendung der `to_uppercase` Methode oder der `to_ascii_uppercase` Methode erreicht werden. Hier ist ein Beispielcode:

``` Rust
let mut mein_string = String::from("hallo welt");
mein_string = mein_string.to_uppercase();
println!("{}", mein_string); // Ausgabe: HALLO WELT
```

Eine alternative Methode ist die Verwendung von `chars` und `map`. Hier ist ein Beispielcode:

``` Rust
let mut mein_string = String::from("hallo welt");

mein_string = mein_string.chars()
    .map(|c| c.to_ascii_uppercase())
    .collect();
    
println!("{}", mein_string); // Ausgabe: HALLO WELT
```

Eine dritte Möglichkeit ist die Verwendung der `to_uppercase` Funktion aus dem `ascii` Crate. Hier ist ein Beispielcode:

``` Rust
use ascii::{AsciiChar, AsciiString};

let mut mein_ascii_string = AsciiString::from_str("hallo welt");
mein_ascii_string = mein_ascii_string.to_uppercase();
println!("{}", mein_ascii_string); // Ausgabe: HALLO WELT
```

Tiefer eintauchen

Das Kapitalisieren von Strings ist eine gängige Aufgabe in der Programmierung, und nahezu alle Programmiersprachen bieten Möglichkeiten, dies zu erreichen. In früheren Versionen von Rust musste man eine externe Library (wie z.B. `text_io`) verwenden, um dies zu erreichen. Seit Rust 1.27 ist diese Funktion jedoch in die Standardbibliothek integriert.

Alternativ kann das `to_uppercase` Verfahren auch auf einzelne Zeichen angewendet werden. Dies ermöglicht eine feinere Kontrolle über die Formatierung von Strings.

Der konkrete Algorithmus, der zur Kapitalisierung eines Strings verwendet wird, ist vom System abhängig und kann in zukünftigen Rust-Versionen geändert werden.

Weitere Informationen

- Rust Standard Library Dokumentation für `to_uppercase`: https://doc.rust-lang.org/std/string/trait.UpperCase.html
- `ascii` Crate Dokumentation: https://docs.rs/ascii/0.9.1/ascii/
- Frühere Option: `text_io` Crate Dokumentation: https://docs.rs/text_io/0.1.7/text_io/
- Rust-Community-Diskussion über das Kapitalisieren von Strings: https://users.rust-lang.org/t/why-use-ascii-ci-case-insensitive-or-upper-case-or-lower-case/18057