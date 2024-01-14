---
title:    "Rust: Löschen von Zeichen, die einer bestimmten Struktur entsprechen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt mit dem Löschen von Zeichen beschäftigen, die einem bestimmten Muster entsprechen? Eine häufige Situation ist zum Beispiel, dass man in einer Textdatei bestimmte Wörter oder Zeichen entfernen möchte, um einen klareren und saubereren Text zu erhalten. In diesem Blogbeitrag zeigen wir, wie man dies mit Rust sehr effizient umsetzen kann.

## So geht's
Um Zeichen in Rust zu löschen, gibt es verschiedene Wege. Ein einfacher Ansatz ist die Verwendung der "replace()" Funktion aus der "str" Bibliothek. Diese Funktion nimmt ein Muster und einen Ersatz als Parameter und ersetzt alle Vorkommen des Musters durch den Ersatz. Schauen wir uns dazu ein Beispiel an:

```Rust
let text = "Dies ist ein Testtext.";
let neuer_text = text.replace("Test", "");
println!("{}", neuer_text);
// Ausgabe: "Dies ist ein Text."
```
In diesem Beispiel wird das Wort "Test" aus dem Text entfernt und der verbleibende Text wird in der Variable "neuer_text" gespeichert. Durch die Verwendung von "`{}`" in der "println!" Macro wird der Inhalt der Variablen ausgegeben.

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken. Hierbei können komplexere Muster definiert werden, um bestimmte Zeichen zu finden und zu ersetzen. Dies kann mit der "regex" Bibliothek in Rust umgesetzt werden. Hier ein Beispiel:

```Rust
extern crate regex; // Import der Bibliothek
use regex::Regex; // Nutzung der Regex-Struktur

let text = "11:00 Uhr"; // Text mit Uhrzeitangabe
let muster = Regex::new(r"\d\d:\d\d").unwrap(); // Definieren des Musters (2 Zahlen, ein Doppelpunkt, 2 Zahlen)
let neuer_text = muster.replace_all(text, ""); // Alle Vorkommen des Musters werden gelöscht
println!("{}", neuer_text);
// Ausgabe: " Uhr"
```

## Tiefergehende Informationen
Das Löschen von Zeichen oder Textpassagen ist in vielen Situationen hilfreich, um Texte zu bereinigen oder bestimmte Informationen zu extrahieren. In Rust gibt es verschiedene Bibliotheken und Funktionen, die dabei unterstützen. Es kann auch sinnvoll sein, sich mit regulären Ausdrücken zu beschäftigen, um komplexere Muster zu definieren oder Texte auf bestimmte Bedingungen zu überprüfen.

## Weitere Informationen
Sie möchten mehr über das Löschen von Zeichen in Rust erfahren? Hier sind ein paar weitere Ressourcen, die Ihnen helfen könnten:

- Offizielle Rust Dokumentation zu "replace()": https://doc.rust-lang.org/std/primitive.str.html#method.replace
- Rust Bibliothek "regex": https://docs.rs/regex/1.5.4/regex/
- Tutorials zu regulären Ausdrücken in Rust: https://docs.rs/regex/1.5.4/regex/
- Weitere nützliche Funktionen für Textmanipulation in Rust: https://stackoverflow.com/questions/41745899/clean-a-char-based-text-string-in-rust/41762625#41762625