---
title:    "Rust: Die Verwendung von regulären Ausdrücken"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Wenn du ein Programmierer bist, hast du wahrscheinlich schon von regulären Ausdrücken gehört. Aber warum sollte man sich überhaupt mit ihnen beschäftigen? Nun, reguläre Ausdrücke sind unglaublich nützlich, wenn es darum geht, Textmuster in Strings zu finden oder zu ersetzen. Sie können dir viel Zeit und Coding-Aufwand ersparen, insbesondere wenn du mit großen Textmengen arbeitest. In diesem Blog-Beitrag werden wir uns ansehen, wie man reguläre Ausdrücke in der Programmiersprache Rust verwendet. 

## Anleitung

Um reguläre Ausdrücke in Rust zu verwenden, müssen wir zuerst das Rust-Regex-Modul importieren. Füge dazu folgende Zeile am Anfang deines Codes hinzu:

```rust
use regex::Regex;
```

Dann erstellen wir ein Regex-Objekt, das den Ausdruck enthält, nach dem wir suchen möchten. Zum Beispiel, wenn wir nach dem Wort "Hallo" in einem String suchen wollen, würden wir folgendes tun:

```rust
let re = Regex::new("Hallo").unwrap();
```

Als nächstes müssen wir den String angeben, in dem wir suchen wollen. Dafür können wir die match-Methode des erstellten Regex-Objekts verwenden:

```rust
let text = "Hallo Welt!";
let result = re.is_match(text);
```

Die Ergebnisvariable wird dann true sein, wenn das Wort "Hallo" im String vorhanden ist, andernfalls wird sie false sein. Du kannst auch die find-Methode verwenden, um die Position des ersten Vorkommnisses des regulären Ausdrucks im String zu finden:

```rust
let text = "Hallo Welt!";
let result = re.find(text);
```

Dies wird ein Option-Objekt zurückgeben, das entweder None ist, wenn kein Vorkommnis gefunden wurde, oder Some(start, end), wobei start und end die Position des Vorkommnisses im String angeben.

## Tiefergehende Informationen

Jetzt, da wir wissen, wie man reguläre Ausdrücke in Rust verwendet, können wir tiefer in das Thema eintauchen und einige der fortgeschritteneren Funktionen kennenlernen. Eine davon ist das Ersetzen von Text innerhalb eines Strings. Dafür gibt es die replace-Methode, die eine geänderte Version des Strings zurückgibt, in dem alle Vorkommnisse des regulären Ausdrucks durch einen Ersatztext ersetzt wurden:

```rust
let text = "Hallo Welt!";
let result = re.replace_all(text, "Guten Tag");
```

Dies wird "Guten Tag Welt!" als Ergebnis zurückgeben. 

Ein weiteres nützliches Feature ist die Möglichkeit, Gruppen in einem regulären Ausdruck zu definieren. Das ermöglicht es dir, bestimmte Teile des Vorkommnisses zu erfassen und sie in der Ersatztext-Definition zu verwenden. Zum Beispiel, wenn du nach einer Telefonnummer suchst und die Vorwahl und die Nummer getrennt in Klammern angeben möchtest, könntest du folgenden regulären Ausdruck verwenden:

```rust
let re = Regex::new(r"\((\d{3})\)\s(\d{3}-\d{4})").unwrap();
```

Hier sind die runde Klammern die Gruppen, die wir erfassen wollen. Dann können wir mit Hilfe von Variablen diese Gruppen im Ersatztext wiederverwenden:

```rust
let text = "Meine Nummer ist (123) 456-7890";
let result = re.replace_all(text, "Meine Nummer ist $1-$2");
```

Das würde "Meine Nummer ist 123-456-7890" als Ergebnis zurückgeben.

## Siehe auch

- Offizielle Rust-Dokumentation zu regulären Ausdrücken: https://doc.rust-lang.org/std/vec/struct.Vec.html
- Eine ausführliche Einführung in reguläre Ausdrücke in Rust: https://tutorialedge.net/rust/rust-regular-expressions-tutorial/
- Teste deine regulären Ausdrücke online mit diesem interaktiven Tool: https://regexr.com/