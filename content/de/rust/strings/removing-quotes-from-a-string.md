---
date: 2024-01-26 03:41:32.290827-07:00
description: "Das Entfernen von Anf\xFChrungszeichen aus einem String in Rust geht\
  \ darum, unn\xF6tige zus\xE4tzliche Anf\xFChrungszeichen, die m\xF6glicherweise\
  \ um Ihre Textdaten\u2026"
lastmod: '2024-03-13T22:44:53.660653-06:00'
model: gpt-4-0125-preview
summary: "Das Entfernen von Anf\xFChrungszeichen aus einem String in Rust geht darum,\
  \ unn\xF6tige zus\xE4tzliche Anf\xFChrungszeichen, die m\xF6glicherweise um Ihre\
  \ Textdaten\u2026"
title: "Anf\xFChrungszeichen aus einem String entfernen"
---

{{< edit_this_page >}}

## Was & Warum?

Das Entfernen von Anführungszeichen aus einem String in Rust geht darum, unnötige zusätzliche Anführungszeichen, die möglicherweise um Ihre Textdaten eingefasst sind, wegzunehmen. Programmierer tun dies, wenn sie Strings bereinigen oder normalisieren müssen, vielleicht nachdem sie Daten aus einer Datei geparst haben, oder wenn sie diese für ein anderes Format vorbereiten, in dem Anführungszeichen problematisch oder überflüssig sein könnten.

## Wie man es macht:

```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Hallo, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Ausgabe: Hallo, Rustaceans!
}
```

Manchmal haben Sie einen String mit gemischten Anführungszeichen, wie diesen:

```Rust
fn main() {
    let mixed_quoted = "'Rust sagt: \"Hallo, Welt!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Ausgabe: Rust sagt: "Hallo, Welt!"
}
```

Hier werden nur die äußersten einfachen Anführungszeichen entfernt.

## Tiefer Einblick

Wenn man Anführungszeichen aus einem String entfernt, könnte man sich fragen, warum es nicht einfach ein `.replace("\"", "")` ist. Frühzeitig war der Umgang mit Text weniger standardisiert, und verschiedene Systeme hatten unterschiedliche Methoden, Text zu speichern und zu übertragen, oft mit einer Art von 'Escape-Sequenz' für spezielle Charaktere. Die `trim_matches`-Methode von Rust ist vielseitiger und ermöglicht es Ihnen, mehrere Charaktere zum Trimmen anzugeben und ob von Anfang (Präfix), Ende (Suffix) oder beiden Seiten des Strings getrimmt werden soll.

Es gibt natürlich Alternativen. Regex ist das Kraftpaket für die String-Manipulation, fähig, komplexe Muster zu erkennen, und wäre für das bloße Entfernen von Anführungszeichen zu viel des Guten. Bibliotheken wie `trim_in_place` könnten das In-Place-Trimmen ohne den Overhead der Erstellung eines neuen `String`-Objekts anbieten, was für leistungskritische Anwendungen wünschenswert sein könnte.

Unter der Haube geht `trim_matches` tatsächlich die Charaktere des Strings von beiden Enden durch, prüft sie gegen das bereitgestellte Muster, bis ein nicht übereinstimmender Charakter gefunden wird. Es ist effizient für das, was es tut, aber seien Sie sich immer bewusst, dass es mit Unicode-Skalarwerten arbeitet. Wenn Ihr String mehrbyte Unicode-Charaktere enthalten könnte, müssen Sie sich keine Sorgen machen, dass sie aufgetrennt werden.

## Siehe auch

- Rusts Dokumentation zur String-Manipulation: https://doc.rust-lang.org/book/ch08-02-strings.html
- Die `regex`-Crate für komplexe Muster: https://crates.io/crates/regex
- Rust anhand von Beispielen für praktische Programmierszenarien: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
