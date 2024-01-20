---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die String-Interpolation in Rust ermöglicht es, Variablen direkt in Zeichenketten einzubetten. Sie macht den Code lesbarer und die Formatierungsaufgaben einfacher.

## So geht's:

Hier ist ein einfacher Code-Abschnitt, der zeigt, wie Sie in Rust String-Interpolation durchführen:

```Rust
let name = "Fritz";
let greeting = format!("Hallo, {}!", name);
println!("{}", greeting);
```

Die Ausgabe wird sein: 

```
Hallo, Fritz!
```

## Vertiefung

Erstens historischer Kontext: obwohl Rust eine relativ neue Sprache ist, wurde das Konzept der String-Interpolation von älteren Sprachen wie Perl und PHP übernommen. Es verbessert die Lesbarkeit des Codes massiv und erleichtert die Arbeit mit dynamischen Strings.

Zweitens Alternativen: Rust bietet auch die Methode `push_str()` für das Anhängen von Strings und die Methode `+` für das Verknüpfen von Strings. Beide erfordern jedoch mehr Code und sind weniger lesbar als die String-Interpolation.

Drittens Implementierungsdetails: Die Funktion `format!()` in Rust wird zur String-Interpolation verwendet. Sie funktioniert ähnlich wie `println!()`, gibt aber einen String zurück, anstatt ihn auszudrucken.

## Weiterführende Links

Einige hilfreiche Links zu diesem Thema:
- Die offizielle [Rust Dokumentation](https://doc.rust-lang.org/stable/rust-by-example/std/str.html) über Zeichenketten.
- Eine [StackOverflow-Diskussion](https://stackoverflow.com/questions/29483365/what-is-the-format-macro-in-rust) über die `format!()` Funktion in Rust.