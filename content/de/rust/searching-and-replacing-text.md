---
title:    "Rust: Suchen und Ersetzen von Text"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Rust ist eine moderne Programmiersprache, die sich durch ihre Geschwindigkeit, Sicherheit und Einfachheit auszeichnet. Wenn Sie also ein Projekt haben, bei dem Sie große Mengen an Text suchen und ersetzen müssen, ist Rust eine ausgezeichnete Wahl. In diesem Blog-Post werde ich Ihnen zeigen, wie Sie mithilfe der leistungsstarken Funktionen von Rust effizient und fehlerfrei Text suchen und ersetzen können.

## Wie geht das?

Um Text in Rust zu suchen und zu ersetzen, verwenden wir die Funktion `replace` aus der Standardbibliothek `std::string::String`. Diese Funktion erwartet zwei Argumente: ein Suchmuster und ein Ersatzmuster. Nehmen wir an, wir haben einen String `text` und möchten alle Vorkommnisse von "Hallo" durch "Guten Tag" ersetzen. Der Code dafür sieht wie folgt aus:

```Rust
let text = String::from("Hallo Welt! Hallo Rust!");
let new_text = text.replace("Hallo", "Guten Tag");
println!("{}", new_text);
```

Die Ausgabe wird wie erwartet "Guten Tag Welt! Guten Tag Rust!" sein. Wie Sie sehen, ist die Syntax in Rust sehr intuitiv und selbsterklärend.

## Tiefer eintauchen

Die `replace` Funktion verwendet interne Methoden, um effizient nach dem Suchmuster zu suchen und es durch das Ersatzmuster zu ersetzen. Sie können auch bestimmte Vorkommnisse überspringen oder nur das erste Vorkommnis ersetzen, indem Sie zusätzliche Argumente übergeben. Weitere Informationen zur Funktionsweise von `replace` und anderen nützlichen Funktionen zum Suchen und Ersetzen von Text finden Sie in der offiziellen Rust-Dokumentation.

## Siehe auch

- [Offizielle Rust-Dokumentation](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Rust-Programmierung: Eine moderne Einführung](https://www.oreilly.com/library/view/programming-rust/9781491927236/)
- [Rust für Jedermann: Eine praktische Einführung in die Programmierung](https://www.addison-wesley.de/9783827242004/rust-fur-jedermann/)