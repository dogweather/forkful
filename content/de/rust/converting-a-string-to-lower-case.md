---
title:    "Rust: Eine Zeichenfolge in Kleinbuchstaben umwandeln"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Zeichenketten in Kleinbuchstaben ist eine häufig benötigte Funktion in der Programmierung. Es kann hilfreich sein, um Benutzereingaben zu standardisieren oder um Vergleiche zwischen Zeichenketten durchzuführen.

## Wie geht man vor

In Rust kann man eine Zeichenkette ganz einfach in Kleinbuchstaben umwandeln, indem man die `to_lowercase()` Methode aufruft. Hier ist ein Beispiel:

```Rust
let name = "Max Mustermann";
let lower_case_name = name.to_lowercase();
println!("Name in Kleinbuchstaben: {}", lower_case_name);
// Output: Name in Kleinbuchstaben: max mustermann
```

## Tiefergehende Informationen

Beim Konvertieren von Zeichenketten in Kleinbuchstaben gibt es ein paar Dinge zu beachten. Zum einen unterstützt Rust Unicode-Zeichen, daher kann es vorkommen, dass manche Zeichen bei der Konvertierung nicht einfach nur in einen Kleinbuchstaben umgewandelt werden, sondern dort gibt es spezifische Regeln. Zum anderen kann es bei manchen Zeichenketten zu unerwartetem Verhalten kommen, zum Beispiel wenn sie nicht in einer kompatiblen Codierung vorliegen.

Um diese Probleme zu vermeiden, gibt es in Rust die `fold()` Methode, die eine Funktion als Parameter annimmt, um spezielle Regeln für die Konvertierung festzulegen. Hier ist ein Beispiel:

```Rust
let name = "üÖä";
let lower_case_name = name.fold(String::new(), |mut s, c| {
    // Für jedes Zeichen der Zeichenkette wird nun diese Funktion aufgerufen
    // Hier verwenden wir eine Library-Funktion, um Umlaute in Kleinbuchstaben umzuwandeln
    for lower_c in c.to_lowercase() {
        s.push(lower_c);
    }
    s
});
println!("Name in Kleinbuchstaben: {}", lower_case_name);
// Output: Name in Kleinbuchstaben: uoae
```

## Siehe auch

- [Rust Dokumentation über die `to_lowercase()` Methode](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Rust Dokumentation über die `fold()` Methode](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.fold)
- [Unicode Standard über Konvertierung in Kleinbuchstaben](https://unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)