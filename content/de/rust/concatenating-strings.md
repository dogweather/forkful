---
title:                "Verketten von Zeichenketten."
html_title:           "Rust: Verketten von Zeichenketten."
simple_title:         "Verketten von Zeichenketten."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Strings sind ein wichtiges Konzept in der Programmierung und sie werden oft benötigt, um Daten aussagekräftiger zu machen und das Nutzererlebnis zu verbessern. Das Verketten oder Zusammensetzen von Strings ist eine gängige Aufgabe, die es ermöglicht, mehrere Strings zu einem zusammenzufügen und so eine neue, kombinierte String zu erstellen. Diese Funktion ist besonders nützlich, wenn es darum geht, dynamische Texte zu erstellen, zum Beispiel in einer Benutzeroberfläche oder in der Ausgabe von Daten.

## Wie geht man vor?

Die Verkettung von Strings in Rust kann auf verschiedene Arten erfolgen, abhängig von den zu verwendenden Daten. Hier sind einige gebräuchliche Methoden:

1. Die `+` Operator-Methode: Diese Methode verwendet den `+` Operator, um zwei Strings zusammenzufügen. Der `+` Operator ist in Rust definiert, um die ```add()``` Methode zu verwenden, die Strings verkettet. Hier ist ein Beispiel:

```Rust
let name = "Max";
let greeting = "Hallo, ";
let message = greeting + name;
println!("{}", message);
```

Die Ausgabe wäre: "Hallo, Max".

2. Die `format!()`-Methode: Diese Methode ermöglicht es, mehrere Werte in eine String-Vorlage einzufügen und sie so zu kombinieren. Hier ist ein Beispiel:

```Rust
let name = "Max";
let age = 25;
let message = format!("Mein Name ist {} und ich bin {} Jahre alt.", name, age);
println!("{}", message);
```

Die Ausgabe wäre: "Mein Name ist Max und ich bin 25 Jahre alt."

Es ist wichtig anzumerken, dass diese Methode die Werte nicht direkt in den String einfügt, sondern eine neue String-Instanz erstellt, die die eingesetzten Werte enthält.

## Tiefere Einblicke

Es gibt verschiedene Ansätze, um Strings in Rust zu verketten, je nach Anwendungsfall und Leistungsoptimierung. Hier sind einige zusätzliche Informationen, die bei der Verwendung von Strings in Rust nützlich sein können:

- Strings in Rust sind standardmäßig unveränderbar, was bedeutet, dass sie nach der Erstellung nicht mehr geändert werden können. Daher erstellt jeder Verkettungsvorgang eine neue String-Instanz, anstatt die ursprüngliche zu ändern.
- Es ist auch möglich, mehrere Strings mit der `concat!()` Macro-Methode zu verketteten. Diese Methode funktioniert ähnlich wie die `format!()` Methode, aber sie ist typsicherer und kann zur Kompilierzeit optimiert werden.
- Wenn die Leistung entscheidend ist, kann die Verwendung von Bibliotheken wie `StringBuilder` oder `StringBuffer` empfohlen werden, die eine optimierte und effizientere String-Verkettung ermöglichen.

## Siehe auch

- [Rust-Dokumentation zu String-Verkettung](https://doc.rust-lang.org/book/ch08-03-hash-maps.html#using-hash-maps)
- [Offizielle Rust-Website](https://www.rust-lang.org/)
- [Rust-Lernmaterial auf Deutsch](https://wiki.openstreetmap.org/wiki/Rust#Lernmaterial)