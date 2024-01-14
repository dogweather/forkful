---
title:                "Rust: Löschen von Zeichen, die einem Muster entsprechen."
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine häufige Aufgabe in der Programmierung. Mit der Sprache Rust wird dies jedoch besonders effizient und einfach zu implementieren. In diesem Blogbeitrag zeigen wir, warum es sich lohnt, diese Methode zu nutzen.

## Wie geht man vor

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können wir die `trim_matches` Funktion aus der Standardbibliothek von Rust verwenden. Diese Funktion durchläuft einen gegebenen String und entfernt alle Zeichen, die dem angegebenen Muster entsprechen.

Hier ist ein Beispiel, wie wir diese Funktion verwenden können:

```Rust 
let text = "Hello!!!";
let result = text.trim_matches('!');
println!("{}", result);
```

In diesem Beispiel wird der Text "Hello!!!" eingelesen und dann mittels `trim_matches` alle Ausrufezeichen entfernt. Das Ergebnis ist dann "Hello".

Eine weitere Möglichkeit ist die Verwendung der `replace` Funktion. Diese ersetzt alle Vorkommen eines bestimmten Musters durch ein anderes Zeichen oder einen anderen String. Hier ist ein Beispiel:

```Rust 
let text = "Hello!!!";
let result = text.replace('!', '');
println!("{}", result);
```

In diesem Fall wird wieder der Text "Hello!!!" eingelesen, aber alle Ausrufezeichen werden durch leere Strings ersetzt, was also zu "Hello" als Ergebnis führt.

In beiden Fällen können wir natürlich auch beliebige andere Muster angeben, die gelöscht werden sollen. Es ist auch möglich, mehrere Muster in einem Durchlauf zu löschen, indem man diese in einer Liste angibt, beispielsweise `trim_matches(&['!', '?'])`.

## Tieferer Einblick

Im Hintergrund nutzt die `trim_matches` Funktion die `Pattern` Trait aus der Rust Standardbibliothek, um die Zeichen zu finden, die dem angegebenen Muster entsprechen. Dies ermöglicht es uns, auch komplexere Muster zu verwenden, wie zum Beispiel reguläre Ausdrücke.

Die `Pattern` Trait definiert die `matches` Methode, die überprüft, ob ein gegebenes Zeichen dem Muster entspricht oder nicht. Dies wird dann von `trim_matches` genutzt, um das String zu durchlaufen und die entsprechenden Zeichen zu löschen.

Es ist auch möglich, selbst ein eigenes Muster zu definieren, indem man dieses Trait implementiert. Dies bietet uns die Flexibilität, maßgeschneiderte Lösungen für unsere spezifischen Anforderungen zu erstellen.

## Siehe auch

- [Die Rust Standardbibliothek](https://doc.rust-lang.org/std/index.html)
- [Die Pattern Trait Dokumentation](https://doc.rust-lang.org/std/pattern/trait.Pattern.html)
- [Rust By Example: String Manipulation](https://doc.rust-lang.org/stable/rust-by-example/std/str.html#string-manipulation)