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

Was und Warum?

Das Schreiben auf den Standardfehler ist eine Möglichkeit für Programmierer, Fehlermeldungen und andere wichtige Informationen während der Laufzeit ihres Programms anzuzeigen. Es gibt uns die Möglichkeit, kritische Informationen auf eine separate Ausgabe zu senden, anstatt sie zwischen den regulären Programmausgaben zu verlieren.

Wie zu:

```Rust
eprintln!("Dies ist eine Fehlermeldung!");
```

Standardfehler kann in Rust mit der Funktion `eprintln!` aufgerufen werden. Wir können dem Aufruf auch eine Zeichenkette übergeben, die die Fehlermeldung oder andere relevante Informationen enthält. Diese Zeichenkette wird dann auf den Standardfehler ausgegeben.

Wenn wir unser Programm ausführen, sehen wir die Zeichenkette auf der Konsole anders ausgegeben als die regulären Programmausgaben:

```Rust
Dies ist eine Fehlermeldung!
```

Tiefe Tauchgänge:

Es hat sich erwiesen, dass das Schreiben auf den Standardfehler eines der nützlichsten Tools für die Fehlerbehandlung in Programmiersprachen ist. Es ermöglicht uns, wichtige Informationen zu erhalten, ohne das Programm zu unterbrechen oder die Ausgaben zu verfälschen.

Es gibt auch Alternativen, um auf den Standardfehler zu schreiben, wie zum Beispiel die Funktion `error!` in der Standardbibliothek von Rust. Diese Funktion ermöglicht es uns, Fehlermeldungen nach Bedarf zu formatieren, anstatt nur eine Zeichenkette auszugeben.

Die Umsetzung des Schreibens auf den Standardfehler erfolgt in Rust auf systemunabhängige Weise, was bedeutet, dass es in verschiedenen Betriebssystemen funktionieren sollte, ohne dass zusätzlicher Code erforderlich ist.

Siehe auch:

- Dokumentation der `eprintln!` Funktion in der Rust-Standardbibliothek: https://doc.rust-lang.org/std/macro.eprintln.html
- Artikel über das Schreiben auf den Standardfehler in verschiedenen Programmiersprachen: https://www.jstorimer.com/blogs/workingwithcode/7766093-when-to-use-stderr-instead-of-stdout