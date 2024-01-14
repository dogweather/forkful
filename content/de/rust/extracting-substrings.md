---
title:    "Rust: Untersuchen von Teilsequenzen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Rust hat sich schnell zu einer der beliebtesten Programmiersprachen entwickelt, und das aus gutem Grund. Die strenge Typprüfung und die kontrollierte Speicherzuweisung ermöglichen es Entwicklern, leistungsstarke und sichere Anwendungen zu erstellen. Eine häufige Aufgabe bei der Arbeit mit Strings ist die Extrahierung von Teilstrings oder Substrings. In diesem Blog-Beitrag lernen wir, wie man Substrings in Rust extrahiert und warum dies nützlich sein kann.

## Wie geht's?

Zunächst müssen wir unsere Rust-Anwendung erstellen und das `std::str::Chars`-Modul importieren, das uns Zugriff auf die einzelnen Zeichen eines Strings bietet.

```Rust
let string = "Hallo, Welt!";

for character in string.chars() {
    println!("{}", character);
}
```

Dieses Beispiel iteriert über jeden Buchstaben in unserem String und gibt ihn aus. Nun wollen wir aber einen konkreten Teil unseres Strings extrahieren. Dafür können wir die Methode `slice()` verwenden, die folgendermaßen aufgerufen wird:

```Rust
let substring = &string[start_index..end_index];
```

Dabei wird der Teilstring von `start_index` bis `end_index` extrahiert. Beachten Sie, dass der `end_index` nicht im Substring enthalten ist. In unserem Beispiel könnte es so aussehen:

```Rust
let string = "Hallo, Welt!";
let substring = &string[0..3];

println!("{}", substring); // "Hal"
```

Alternativ können wir auch den `len()`-Methode des Strings eine Startposition übergeben, um das Gleiche zu erreichen:

```Rust
let substring = &string[start_index..string.len()];
```

Nun wollen wir aber auch den Rest unseres Strings nach einem bestimmten Zeichen durchsuchen, z.B. nach dem Komma. Dafür gibt es die Methode `find()`, die uns die Position des ersten gefundenen Zeichens zurückgibt, oder `rfind()`, die von rechts sucht.

```Rust
let index = string.find(',');
let substring = &string[start_index..index];
```

Weitere nützliche Methoden für die Arbeit mit Substrings sind `trim()`, um Leerzeichen am Anfang und Ende zu entfernen, und `split()`, um den String an einem bestimmten Trennzeichen aufzuteilen.

## Tiefer eintauchen

Rust bietet auch die Möglichkeit, über Pattern-Matching Substrings zu extrahieren. Dafür können wir das `match`-Statement verwenden und ein `&str`-Pattern angeben, das unseren Suchkriterien entspricht.

```Rust
match string {
    "Hello, world" => println!("Found it!"),
    _ => println!("Not found.")
}
```

Alternativ können wir auch reguläre Ausdrücke verwenden, um noch komplexere Muster zu durchsuchen. Dafür müssen wir das `regex`-Modul in unser Projekt importieren. Ein Beispiel könnte so aussehen:

```Rust
use regex::Regex;

let re = Regex::new(r"(\d{2}-[A-Za-z]{3}-\d{4})").unwrap();
let substring = re.captures(string).unwrap().get(0).unwrap().as_str();
```

Dieses Beispiel zeigt, wie wir mit regulären Ausdrücken ein bestimmtes Datumsmuster aus einem String extrahieren können.

## Siehe auch

* [Offizielle Rust-Dokumentation zu Substrings](https://doc.rust-lang.org/std/primitive.str.html#method.slice)
* [Rust By Example: Strings and Substrings](https://doc.rust-lang.org/rust-by-example/std/str.html)
* [Reguläre Ausdrücke in Rust](https://docs.rs/regex/)

Vielen Dank fürs Lesen und viel Spaß beim Extrahieren von Substrings in Rust!