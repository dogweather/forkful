---
title:    "Rust: Suchen und Ersetzen von Text."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum
Hast du jemals versucht, einen bestimmten Text in einem Dokument zu finden und durch einen anderen zu ersetzen? Dies kann ein langwieriger Prozess sein, besonders wenn der Text mehr als einmal vorkommt. Aber zum Glück bietet Rust eine effiziente Möglichkeit, Text zu suchen und zu ersetzen. In diesem Blog-Beitrag werde ich dir zeigen, wie es gemacht wird.

## Wie geht das?
Um Text in Rust zu suchen und zu ersetzen, können wir die `replace()` Methode verwenden. Sie erlaubt es uns, einen gefundenen Text mit einem anderen Text zu ersetzen. Schauen wir uns ein Beispiel an:

```Rust
let original_text = "Hello, world! Hello, universe!";

let modified_text = original_text.replace("Hello", "Hi");

println!("{}", modified_text);
```

Die Ausgabe dieses Codes wird sein: "Hi, world! Hi, universe!". Wie du sehen kannst, ersetzt die `replace()` Methode alle Vorkommen von "Hello" durch "Hi". Wir können auch angeben, wie viele Vorkommen wir ersetzen möchten, indem wir einen optionalen dritten Parameter angeben:

```Rust
let original_text = "Hello, world! Hello, universe!";

let modified_text = original_text.replace("Hello", "Hi", 1);

println!("{}", modified_text);
```

Die Ausgabe wird jetzt sein: "Hi, world! Hello, universe!". Der erste Parameter bezeichnet den zu ersetzenden Text, der zweite Parameter den neuen Text und der dritte Parameter gibt an, wie oft der Austausch durchgeführt werden soll.

## Tiefer Einblick
Die `replace()` Methode funktioniert mit Zeichenketten, aber auch mit Zeichen. Sowohl der zu ersetzende Text als auch der neue Text können aus mehreren Zeichen bestehen. Zudem können wir der Methode auch eine [`Regex`](https://docs.rs/regex/latest/regex/) als Parameter übergeben, um komplexere Suchen und Ersetzungen durchzuführen.

## Siehe auch
- [Rust Standardbibliothek](https://doc.rust-lang.org/std/)
- [Rust Regex-Dokumentation](https://docs.rs/regex/latest/regex/)