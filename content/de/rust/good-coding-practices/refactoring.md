---
date: 2024-01-26 03:36:53.156146-07:00
description: "Refactoring ist der Prozess der Umstrukturierung vorhandenen Computer-Codes\
  \ \u2013 also die \xC4nderung der Faktoren \u2013, ohne sein externes Verhalten\
  \ zu ver\xE4ndern.\u2026"
lastmod: '2024-03-11T00:14:27.572325-06:00'
model: gpt-4-0125-preview
summary: "Refactoring ist der Prozess der Umstrukturierung vorhandenen Computer-Codes\
  \ \u2013 also die \xC4nderung der Faktoren \u2013, ohne sein externes Verhalten\
  \ zu ver\xE4ndern.\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Was & Warum?

Refactoring ist der Prozess der Umstrukturierung vorhandenen Computer-Codes – also die Änderung der Faktoren –, ohne sein externes Verhalten zu verändern. Programmierer führen dies durch, um nichtfunktionale Attribute der Software, wie Lesbarkeit, reduzierte Komplexität, verbesserte Wartbarkeit und die Schaffung einer ausdrucksstärkeren internen Architektur oder eines Objektmodells zur Verbesserung der Erweiterbarkeit, zu verbessern.

## Wie:

Lassen Sie uns ein einfaches Stück Rust-Code refaktorisieren, um es idiomatischer und wartbarer zu machen. Wir beginnen mit einer Funktion, die die Summe eines Vektors von Ganzzahlen berechnet:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Die Summe ist {}", sum(&numbers));
}
```

Ausgabe:
```
Die Summe ist 15
```

Jetzt wollen wir dies so refaktorisieren, dass wir idiomatischeres Rust durch die Nutzung von Iteratoren und der `fold`-Methode verwenden:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Die Summe ist {}", sum(&numbers));
}
```

Keine Änderung in der Ausgabe – es ist immer noch `15` –, aber die refaktorisierte Version ist sauberer und nutzt die Stärken von Rust, wie Ausleihen und Iterator-Methoden.

## Tiefere Einblicke

Refactoring hat seine Wurzeln in der Smalltalk-Community und wurde in der Java-Welt durch Martin Fowlers Buch „Refactoring: Improving the Design of Existing Code” popularisiert. Seine Prinzipien sind universell und gelten auch für Rust, wo Sicherheit und Parallelität von größter Bedeutung sind. Rust fördert das Schreiben von robustem Code, indem es Probleme bereits zur Kompilierzeit aufdeckt, sodass der Rust-Compiler beim Refactoring als Sicherheitsnetz fungiert.

Alternativen zum manuellen Refactoring umfassen die Verwendung von automatisierten Werkzeugen, wie ‚rustfmt‘ für die Codeformatierung und ‚clippy‘ für das Linting, die idiomatischere Schreibweisen des Codes vorschlagen können. Tiefgreifendes Refactoring erfordert jedoch oft ein durchdachtes Verständnis des Code-Designs, was diese Werkzeuge nicht vollständig automatisieren können.

Beim Refactoring in Rust kann es darum gehen, die Nutzung von Typen zu verbessern, Lebenszeiten effektiv zu nutzen, unnötige Allokationen zu reduzieren oder Parallelitätsmuster wie `Arc<Mutex<T>>` bei Bedarf zu verwenden. Es ist auch üblich, von `unwrap()` zu ausdrucksstärkerer Fehlerbehandlung mit `Result<T, E>` zu wechseln.

## Siehe auch

Um tiefer in das Refactoring in Rust einzutauchen:

- Das Rust Buch: https://doc.rust-lang.org/book/
- Rust by Example: https://doc.rust-lang.org/rust-by-example/
- Clippy, ein Rust-Linting-Werkzeug: https://github.com/rust-lang/rust-clippy
- „Refactoring: Improving the Design of Existing Code“ von Martin Fowler: https://martinfowler.com/books/refactoring.html
