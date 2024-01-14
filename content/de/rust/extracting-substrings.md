---
title:    "Rust: Unterstrings extrahieren"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Substring-Extraktion ist eine wichtige Technik beim Programmieren. Es kann verwendet werden, um einen Teil einer Zeichenkette zu extrahieren, der für bestimmte Operationen benötigt wird, anstatt die gesamte Zeichenkette zu verarbeiten. Dadurch können Programme effizienter und schneller ausgeführt werden.

## Wie man Substrings extrahiert

Um Substrings in Rust zu extrahieren, können wir die `get`-Methode von `str` verwenden. Diese Methode erwartet zwei Indizes, die den Anfang und das Ende des zu extrahierenden Substrings angeben.

Eine Beispielanwendung könnte so aussehen:

```Rust
let s = "Dies ist ein Beispiel";
// extrahiert den Substring von Index 5 bis 10
let substring = s.get(5..10);
println!("{}", substring); // Ausgabe: ist ein
```

Der `get`-Methode kann auch eine `RangeFull` übergeben werden, um den gesamten Inhalt der Zeichenkette zu extrahieren.

Die Ausgabe wäre in diesem Fall "Dies ist ein Beispiel".

## Tiefer Einblick

In Rust werden Substrings als `&str` dargestellt, da sie nur einen Teil einer Zeichenkette enthalten und nicht als eigenständige Zeichenkette betrachtet werden. Diese `&str` können jedoch auch in `String` konvertiert werden, indem sie der `to_string`-Methode übergeben werden.

Wenn wir eine `&str`-Darstellung eines Substrings zu einer bestehenden `String`-Variable hinzufügen, wird der gesamte Inhalt der Zeichenkette kopiert und eine neue `String` erstellt. Dies kann zu Performance-Problemen führen, insbesondere wenn die ursprüngliche Zeichenkette sehr lang ist.

Es ist auch wichtig zu beachten, dass Substrings in Rust immer Zugriff auf die ursprüngliche Zeichenkette benötigen. Wenn die ursprüngliche Zeichenkette gelöscht oder geändert wird, wird der Substring nicht mehr korrekt funktionieren.

## Siehe auch

- [Rust Dokumentation über die get-Methode] (https://doc.rust-lang.org/std/primitive.str.html#method.get)
- [Offizielle Rust Website] (https://www.rust-lang.org/de/)
- [Rust Community Forum] (https://users.rust-lang.org/)