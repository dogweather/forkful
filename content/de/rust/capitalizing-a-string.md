---
title:    "Rust: String großschreiben"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum 

Warum sollte man sich überhaupt mit der Großschreibung einer Zeichenfolge beschäftigen? Nun, es gibt viele Gründe, warum dies eine nützliche Fähigkeit für jeden Rust-Programmierer sein könnte. Zum Beispiel könnte es notwendig sein, einen Benutzernamen oder einen Titel zu formatieren, um eine bestimmte Darstellung zu erreichen. Oder vielleicht müssen Sie Daten für eine externe API bereinigen, die eine bestimmte Groß- und Kleinschreibung erfordert. Unabhängig von der genauen Verwendung, ist es wichtig zu wissen, wie man in Rust eine Zeichenfolge in Großbuchstaben umwandelt.

## Wie man es macht

Es gibt mehrere Möglichkeiten, eine Zeichenfolge in Rust in Großbuchstaben umzuwandeln. Eine Möglichkeit ist die Verwendung von Methoden wie `to_uppercase()` oder `to_ascii_uppercase()` auf einer `String`-Variable. Zum Beispiel:

```
let name = String::from("Max Mustermann"); 
let upper_name = name.to_uppercase(); 
assert_eq!("MAX MUSTERMANN", upper_name); 
```

Es ist auch möglich, die `to_ascii_uppercase()` Methode auf einem `&str` Slice anzuwenden. Dies ist besonders nützlich, wenn Sie mit Benutzereingaben arbeiten. Zum Beispiel:

```
let input = "Das ist eine Testzeichenfolge"; 
let upper_input = input.to_ascii_uppercase(); 
println!("Umgewandelt in Großbuchstaben: {}", upper_input); 
```

Die Ausgabe wäre:

```
UMGEWANDELT IN GROSSBUCHSTABEN: DAS IST EINE TESTZEICHENFOLGE 
```

## Deep Dive 

Eine Zeichenfolge in Großbuchstaben umzuwandeln mag auf den ersten Blick einfach erscheinen, aber es gibt tatsächlich mehrere komplexe Aspekte hinter den Kulissen. Zum Beispiel behandelt Rust Unicode-Zeichen und die verschiedenen Schreibweisen (wie z.B. großgeschrieben, Kleinbuchstaben, oder Titelschreibung) unterschiedlich. Wenn Sie tiefer in dieses Thema eintauchen möchten, empfehle ich Ihnen, die offizielle Rust Dokumentation über Kodierung und Zeichenketten zu lesen.

## Siehe auch 

- [Rust Dokumentation: Kodierung und Zeichenketten](https://doc.rust-lang.org/std/str/index.html) 
- [Rust Dokumentation: String](https://doc.rust-lang.org/std/string/struct.String.html) 
- [Rust Crashkurs: Strings](https://www.rust-crashkurs.de/strings.html)