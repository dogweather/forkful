---
title:                "Einsatz von regulären Ausdrücken"
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster, die in Texten nach spezifischen Sequenzen suchen. Programmierer nutzen sie, um Textdaten schnell zu durchsuchen, zu validieren oder zu manipulieren.

## Wie geht das:
```Rust
extern crate regex;
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b\d{4}\b").unwrap();
    let text = "Das Jahr 2021 war turbulent, aber 2023 sieht besser aus.";
    
    for caps in re.find_iter(text) {
        println!("Gefundene Zahl: {}", caps.as_str());
    }
}
```
Ausgabe:
```
Gefundene Zahl: 2021
Gefundene Zahl: 2023
```

```Rust
extern crate regex;
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+@\w+\.\w+)").unwrap();
    let text = "Meine Email ist jemand@beispiel.de und ich benutze auch info@example.com.";

    let emails: Vec<_> = re.captures_iter(text).filter_map(|cap| {
        cap.get(1).map(|email| email.as_str())
    }).collect();
    
    println!("Gefundene Emails: {:?}", emails);
}
```
Ausgabe:
```
Gefundene Emails: ["jemand@beispiel.de", "info@example.com"]
```

## Deep Dive
Reguläre Ausdrücke (RegEx) haben ihren Ursprung in der theoretischen Informatik und der formalen Sprachtheorie. Sie sind mächtiger als einfaches String-Matching, können aber auch komplex und schwer zu lesen sein. Alternativen wie String-Methoden (`contains`, `startsWith`, `endsWith`) oder Parsing-Bibliotheken (z. B. `nom` in Rust) bieten manchmal einfachere Lösungen, sind jedoch nicht so flexibel wie RegEx. Die Rust `regex`-Bibliothek basiert auf einem Backtracking-Algorithmus und bietet umfangreiche Optimierungen für schnelle Suchvorgänge.

## Siehe auch
- Rust `regex` Dokumentation: https://docs.rs/regex/
- Online RegEx Tester und Debugger: https://regex101.com/
- RegEx Tutorial: https://www.regular-expressions.info/tutorial.html
- Vergleich populärer Parsing-Bibliotheken in Rust: https://lib.rs/search?q=parsing
