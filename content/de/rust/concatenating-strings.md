---
title:                "Rust: Zusammenführen von Zeichenketten"
simple_title:         "Zusammenführen von Zeichenketten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Die Verkettung von Strings ist ein verbreitetes Konzept in der Programmierung, das auch in Rust häufig verwendet wird. Durch das Verbinden von mehreren Strings lässt sich Text auf einfache und effiziente Weise manipulieren und formatieren. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie dies in der Programmiersprache Rust funktioniert.

# Wie geht es?

Um Strings in Rust zu verketten, wird die Methode `push_str()` verwendet. Diese Methode fügt einen weiteren String an das Ende des ursprünglichen Strings an. Es ist auch möglich, mehrere Strings zusammenzufügen, indem `push_str()` mehrmals hintereinander aufgerufen wird. Hier ist ein Beispielcode, der zwei Strings zu einem einzigen String verbindet:

```Rust
let mut string_1 = String::from("Hallo");
let string_2 = String::from("Welt");
string_1.push_str(" ");
string_1.push_str(&string_2);
println!("{}", string_1);
```

Die erste Zeile erstellt einen mutierbaren String namens `string_1`, der den Text "Hallo" enthält. Dann wird ein weiterer String namens `string_2` erstellt, der den Text "Welt" enthält. Mit `push_str()` wird nun ein Leerzeichen an `string_1` angehängt, gefolgt von `string_2`. Das Ergebnis des Programms ist "Hallo Welt".

# Tief Tauchen

Neben der `push_str()` Methode gibt es in Rust auch die `+` Operator Überladung, um Strings zu verketten. Dies funktioniert ähnlich wie bei numerischen Werten und erzeugt einen neuen String, anstatt den ursprünglichen zu ändern. Hier ist ein Beispielcode, der den `+` Operator verwendet:

```Rust
let string_1 = String::from("Hallo");
let string_2 = String::from("Welt");
let new_string = string_1 + " " + &string_2;
println!("{}", new_string);
```

Dieser Code erzeugt den gleichen String "Hallo Welt" wie im vorherigen Beispiel, ändert aber `string_1` und `string_2` nicht. Außerdem gibt es in Rust auch die `format!()` Makro, das zur Formatierung von Strings verwendet werden kann. Es akzeptiert eine beliebige Anzahl von Argumenten und gibt einen formatierten String zurück. Hier ist ein Beispiel mit `format!()`:

```Rust
let name = "Rust";
let phrase = format!("Hallo, {}!", name);
println!("{}", phrase);
```

Die Ausgabe dieses Codes ist "Hallo, Rust!".

# Siehe auch

- [Offizielle Rust Dokumentation über String Verkettung](https://doc.rust-lang.org/std/string/struct.String.html#method.push_str)
- [Rust By Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [The Rust Programming Language - Kapitel 8: Common Collections](https://doc.rust-lang.org/book/ch08-00-common-collections.html)