---
title:    "Rust: Strings zusammenfügen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Die Verkettung von Strings ist eine wichtige Methode in der Programmierung, die es ermöglicht, mehrere Textabschnitte zu einem Ganzen zu vereinen. Dadurch können komplexe Texte erstellt werden, die dynamisch auf Variablen und Eingaben reagieren. In diesem Blog-Beitrag werden wir uns anschauen, wie man diese Funktion in der Programmiersprache Rust verwendet.

## Wie geht's

Das Verketten von Strings in Rust ist sehr intuitiv und einfach. Hier ist ein Beispiel, wie man einen Satz aus mehreren Teilen zusammenfügen kann:

```Rust
let vorname = "Max";
let nachname = "Mustermann";

let vollstaendiger_name = format!("Mein Name ist {} {}", vorname, nachname);

println!("{}", vollstaendiger_name);
```

Die Ausgabe dieses Codes wäre "Mein Name ist Max Mustermann". Wie man sehen kann, wird die Funktion `format!()` verwendet, um die Strings zusammenzufügen. Man gibt zunächst einen String mit den Platzhaltern `{}` an und danach die Variablen in der Reihenfolge, in der sie im Text erscheinen sollen. Für jeden Platzhalter muss also auch eine entsprechende Variable angegeben werden. Diese Methode kann auch benutzt werden, um mehrere Textabschnitte miteinander zu verketten, nicht nur Variablen.

## Tiefere Einblicke

In Rust gibt es mehrere Möglichkeiten, um Strings zu verketten. Eine davon ist die oben gezeigte Methode `format!()`. Eine andere Möglichkeit ist die Verwendung des `+` Operators. Hier ist ein Beispiel:

```Rust
let vorname = "Max";
let nachname = "Mustermann";

let vollstaendiger_name = vorname.to_string() + " " + nachname;

println!("{}", vollstaendiger_name);
```

Auch hier wird der vollständige Name ausgegeben, jedoch wird diesmal der `+` Operator verwendet. Es ist wichtig zu beachten, dass dafür die Methode `.to_string()` aufgerufen werden muss, da der `+` Operator nur mit Strings funktioniert.

Generell ist es in Rust wichtig, die verschiedenen Typen von Variablen zu beachten, da sie unterschiedliche Methoden für die Verkettung von Strings haben können.

## Siehe auch

- [Rust Dokumentation über Strings](https://doc.rust-lang.org/std/string/index.html)
- [Offizielle Rust Webseite](https://www.rust-lang.org/de-DE/)
- [Rust-Tutorial für Einsteiger](https://www.rust-lang.org/de-DE/learn)