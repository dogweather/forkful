---
title:                "Schreiben auf Standardfehler"
html_title:           "Rust: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man etwas auf die Standardfehlerausgabe schreiben? Nun, es ist eine effektive Möglichkeit, Fehlermeldungen und Warnungen aus Ihrem Programm zu überwachen und diese Informationen an Entwickler oder Benutzer weiterzugeben.

## Wie geht das?
Um auf die Standardfehlerausgabe zu schreiben, müssen Sie das Modul `std::io` importieren und die Funktion `io::stderr()` verwenden, um einen Handle auf die Standardfehlerausgabe zu erhalten. Dann können Sie die Funktion `write()` verwenden, um eine Nachricht auf die Standardfehlerausgabe zu schreiben. Hier ist ein Beispielcode:

```Rust
use std::io;
use std::io::Write;

fn main() {
    let mut stderr = io::stderr();
    stderr.write(b"Dies ist eine Fehlermeldung.").unwrap();
}
```
Das `write()` Funktion nimmt eine Schnittstelle als Argument, daher nutzen wir hier `b` um unsere Nachricht in ein Byte-Slice zu konvertieren.

Die Ausgabe würde so aussehen:

```Rust
Dies ist eine Fehlermeldung.
```

## Tieferer Einblick
Wussten Sie, dass die Standardfehlerausgabe auch in den `println!` und `eprintln!` Makros verwendet werden kann? Diese Makros schreiben ihre Argumente zur Standardausgabe beziehungsweise zur Standardfehlerausgabe und fügen automatisch einen Zeilenumbruch hinzu. So können Sie Ihre Fehlermeldungen und Warnungen etwas eleganter gestalten. Hier ist ein Beispiel:

```Rust
fn main() {
    let num = 42;
    eprintln!("Oh nein! Die Zahl {} ist zu groß.", num);
    println!("Aber keine Sorge, ich ignoriere einfach alle Zahlen über 10.");
}
```

Die Ausgabe würde so aussehen:

```Rust
Oh nein! Die Zahl 42 ist zu groß.
Aber keine Sorge, ich ignoriere einfach alle Zahlen über 10.
```

See Also
- [std::io - Rust Standard Library](https://doc.rust-lang.org/std/io/)
- [Rust By Example: I/O](https://doc.rust-lang.org/rust-by-example/std_misc/io.html)
- [The Rust Book: Standard Input and Output](https://doc.rust-lang.org/book/ch09-00-error-handling.html)