---
title:                "Einen String interpolieren"
html_title:           "Rust: Einen String interpolieren"
simple_title:         "Einen String interpolieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
"String interpolieren" ist eine Technik, bei der Werte in einen String eingefügt werden, anstatt sie einfach hintereinander zu schreiben. Programmierer benutzen dies, um Texte dynamisch zu gestalten und sie an bestimmte Variablen oder Einstellungen anzupassen.

## Wie geht das?
In Rust wird String Interpolation mit Hilfe des `format!` Makros durchgeführt. Hier ist ein Beispiel, das einen String mit dem Namen und Alter einer Person erstellt:

```Rust
let name = "Max";
let age = 25;
let output = format!("Mein Name ist {} und ich bin {} Jahre alt.", name, age);
```

Das resultierende `output` wäre dann "Mein Name ist Max und ich bin 25 Jahre alt."

## Tiefere Einblicke
String Interpolation ist eine praktische Methode, um Texte effektiver und flexibler zu gestalten. Es wird oft benutzt in Situationen, wo variable Werte in einen Text eingefügt werden müssen, z.B. bei der Generierung von Benachrichtigungen oder das Erstellen von dynamischen Fehlermeldungen. Alternativen zu String Interpolation in Rust sind z.B. mit Hilfe des `println!` Makros oder durch die Verwendung von `String` und `&str` Typen. Die Implementation von String Interpolation in Rust basiert auf der `std::fmt` Bibliothek.

## Siehe auch
Weitere Informationen zum `format!` Makro und String Interpolation in Rust finden Sie in der [offiziellen Dokumentation](https://doc.rust-lang.org/std/fmt/index.html). Für fortgeschrittenere Techniken und Beispiele, schauen Sie sich diesen [Blogbeitrag](https://codeburst.io/string-interpolation-in-rust-5d7abfaedb76) an.