---
title:                "Ausgabe von Debug-Meldungen"
html_title:           "Gleam: Ausgabe von Debug-Meldungen"
simple_title:         "Ausgabe von Debug-Meldungen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debuggen ist ein unverzichtbarer Bestandteil des Programmierens. Mit der Fähigkeit, Debug-Ausgaben zu drucken, können Entwickler schnell und einfach Fehler in ihrem Code finden und beheben. Lerne, wie du mit Gleam einfach und effizient Debug-Ausgaben drucken kannst.

## Wie

```Gleam
// Ein einfaches Beispiel, um eine Debug-Ausgabe zu drucken
fn main() {
  let name = "Max Mustermann"
  debug::print("Hallo, mein Name ist " ++ name)
}

// Ausgabe: Hallo, mein Name ist Max Mustermann
```

Um eine Debug-Ausgabe in Gleam zu drucken, verwende die `debug::print()` Funktion und übergebe den Wert, den du ausdrucken möchtest, als Argument. Du kannst auch Zeichenketten mit anderen Werten verketten, indem du das `++` Symbol verwendest. Dies ist nützlich, um Variablenwerte in einer aussagekräftigen Debug-Ausgabe zu zeigen.

Um die `debug::print()` Funktion zu nutzen, musst du das `debug` Modul importieren. Dies kannst du tun, indem du am Anfang deiner Datei diese Zeile hinzufügst:

```Gleam
import debug
```

## Deep Dive

Das Drucken von Debug-Ausgaben kann auch dazu beitragen, die Leistung deines Codes zu verbessern. Indem du gezielt nur die wichtigen Informationen ausgibst, kannst du vermeiden, dass dein Code mit unnötigen Ausgaben überladen wird. Gleam bietet auch die Möglichkeit, zu bestimmten Debug-Ausgaben nur im Entwicklungsmodus zu gelangen, so dass sie im Produktionsmodus nicht ausgeführt werden. Dies kann helfen, die Geschwindigkeit und Speichernutzung deines Codes zu optimieren.

Es gibt auch weitere Funktionen im `debug` Modul wie `print!()` und `println!()`, mit denen du Formatierungszeichen verwenden kannst, um deine Debug-Ausgaben noch aussagekräftiger zu machen. Um mehr darüber zu erfahren, empfehle ich dir, die offizielle Gleam-Dokumentation zu lesen.

## Siehe auch

- Offizielle Gleam-Dokumentation: https://gleam.run/book/tour/debugging.html
- Gleam Debugging-Tutorial: https://www.youtube.com/watch?v=RC46wrvDJKc
- Gleam Forum: https://elixirforum.com/c/elixir-forum-announcements/gleam/35