---
title:    "Rust: Ein String großschreiben"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#Warum

In diesem Beitrag werden wir uns ansehen, wie man in Rust eine Zeichenkette großschreibt. Warum sollte man das tun? Das Grossschreiben von Strings ist ein häufiger Schritt bei der Textverarbeitung und kann bei der Validierung von Benutzereingaben oder der Formatierung von Ausgaben hilfreich sein. Außerdem ist es ein gutes Beispiel, um die grundlegenden Konzepte von Rust zu verstehen.

## Wie geht man vor?

Um eine Zeichenkette in Rust großzuschreiben, können wir die Funktion `to_uppercase()` aus dem `std::string`-Modul verwenden. Hier ist ein Beispiel:

```Rust
let text = "Hallo, Welt!";
let uppercased_text = text.to_uppercase();
println!("{}", uppercased_text); // Gibt "HALLO, WELT!" aus
```

In diesem Beispiel erstellen wir eine neue Zeichenkette namens `text` mit dem Inhalt "Hallo, Welt!". Mit der `to_uppercase()`-Funktion konvertieren wir diese Zeichenkette in Großbuchstaben und speichern das Ergebnis in der Variable `uppercased_text`. Dann geben wir das Ergebnis mit `println!()` aus.

Es ist auch möglich, nur einen Teil einer Zeichenkette großzuschreiben, indem man `to_uppercase()` auf ein bestimmtes Zeichen anwendet. Zum Beispiel können wir den ersten Buchstaben eines Wortes großschreiben, indem wir die Methode `to_uppercase()` auf den ersten Buchstaben anwenden:

```Rust
let mut text = String::from("welt");
text[..1].to_uppercase();
println!("{}", text); // Gibt "Welt" aus
```

## Tiefergehende Informationen

Wenn wir uns etwas genauer mit dem Code beschäftigen, können wir feststellen, dass die `to_uppercase()`-Funktion eine `String`-Methode ist, die wiederum auf eine `Chars`-Iterator-Methode angewendet wird. Dies bedeutet, dass wir die Zeichenkette in einzelne Buchstaben aufteilen und jeden Buchstaben in Großbuchstaben konvertieren. Die Konvertierung erfolgt nach den Unicode-Standards und unterstützt auch mehrsprachige Zeichen.

Wenn wir nur einen Teil einer Zeichenkette großschreiben möchten, müssen wir beachten, dass wir `to_uppercase()` auf den Teil der Zeichenkette anwenden, der den `Chars`-Iterator enthält. Deshalb müssen wir die `to_uppercase()`-Methode auf den ersten Buchstaben anwenden, aber nicht auf den Rest der Zeichenkette.

# Siehe auch

- [Offizielle Rust Dokumentation für String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust By Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)