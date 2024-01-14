---
title:                "Rust: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

#Warum

Es gibt viele Gründe, warum Entwickler:innen Debug-Ausgaben verwenden. Hier sind einige davon:

- Debug-Ausgaben können helfen, Fehler in Code zu finden und zu beheben.
- Sie können auch bei der Entwicklung neuer Funktionen nützlich sein, um den Ablauf des Programms zu überprüfen.
- Durch das Ausgeben von Daten können komplexe Algorithmen oder komplexe Logikschleifen besser verstanden werden.
- Und nicht zuletzt können Debug-Ausgaben auch einfach Spaß machen und die Entwicklung unterhaltsamer gestalten.

#Wie geht das

Um Debug-Ausgaben in Rust zu machen, verwenden wir die `println!`-Macro. Es funktioniert ähnlich wie die `printf`-Funktion in C, aber mit Rust-spezifischen Syntax. Hier ist ein einfaches Beispiel:

```Rust
let name = "Max";
println!("Hello, {}!", name);
```

Die Ausgabe wird folgendermaßen aussehen:

```
Hello, Max!
```

Der Text innerhalb der geschweiften Klammern `{}` wird durch den Wert der entsprechenden Variable ersetzt. Dies ist ähnlich wie die Platzhalter in `printf`. 

Aber was ist, wenn wir mehrere Variablen ausgeben wollen? Kein Problem, wir können einfach mehrere `{}`-Platzhalter verwenden:

```Rust
let name = "Max";
let age = 25;
println!("Hello, my name is {} and I am {} years old.", name, age);
```

Die Ausgabe wäre dann:

```
Hello, my name is Max and I am 25 years old.
```

Es gibt jedoch auch fortgeschrittenere Möglichkeiten der Ausgabe, wie z.B. die Verwendung von Formatierungsanweisungen oder das Ausgeben von Datenstrukturen. Wenn du mehr darüber erfahren möchtest, schau dir unbedingt die Rust-Dokumentation zu `println!` an.

#Tiefere Einblicke

Es gibt auch andere Wege, um Debug-Ausgaben zu machen, z.B. die `eprintln!`-Macro, die für Fehler- oder Warnungsmeldungen verwendet werden kann. Außerdem gibt es die Möglichkeit, Debug-Ausgaben nur in bestimmten Entwicklungsumgebungen wie z.B. während des Testens auszuführen, indem man eine Bedingung für die Ausführung der `println!`-Statements hinzufügt.

Eine andere interessante Möglichkeit sind benutzerdefinierte Ausgaben mittels der `Debug`-Trait. Damit können benutzerdefinierte Datenstrukturen oder -typen auf ihre eigene spezifische Weise ausgegeben werden. Dies ist besonders nützlich, wenn es um komplexe Datenstrukturen geht, die nicht einfach mit `println!` ausgegeben werden können.

#Siehe auch

- [Rust-Dokumentation zu `println!`](https://doc.rust-lang.org/std/macro.println.html)
- [Offizielles Rust-Tutorial](https://doc.rust-lang.org/book/)
- [Rust-Tricks: Debug-Ausgaben](https://www.rust-lang.org/learn/tricks/debugging-output)

Danke fürs Lesen und viel Spaß beim Debuggen mit Rust!