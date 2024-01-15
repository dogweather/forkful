---
title:                "Eine String in Kleinbuchstaben umwandeln."
html_title:           "Gleam: Eine String in Kleinbuchstaben umwandeln."
simple_title:         "Eine String in Kleinbuchstaben umwandeln."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man eine Zeichenfolge in Kleinbuchstaben umwandeln möchte. Zum Beispiel, um einen einheitlichen Vergleich bei der Datenverarbeitung zu ermöglichen oder um Nutzereingaben zu standardisieren. Mit Gleam können wir dies schnell und einfach erreichen.

## So geht's

```Gleam
let example_string = "Hallo WELT"
let result = String.to_lower(example_string)
```

Dieses Codebeispiel zeigt, wie wir die Funktion `to_lower` aus dem `String` Modul von Gleam nutzen können, um die Zeichenfolge "Hallo WELT" in "hallo welt" umzuwandeln. Diese Funktion gibt uns immer eine neue Zeichenfolge zurück, um die ursprüngliche Zeichenfolge unverändert zu lassen.

Das Gleam-Team hat auch dafür gesorgt, dass diese Methode Unicode-Zeichen richtig verarbeiten kann, was bei vielen anderen Programmiersprachen nicht der Fall ist. Das bedeutet, dass unsere Ergebnisse immer akkurat und konsistent sind, unabhängig von der verwendeten Zeichenmenge.

## Tiefer Einblick

Eine Zeichenfolge in Kleinbuchstaben umzuwandeln, klingt vielleicht einfach, aber es gibt einige Dinge zu beachten. Gleam verwendet standardmäßig die utf-8 Codierung, um Zeichenfolgen zu speichern. Das bedeutet, dass es auch Zeichen gibt, die aus mehreren Bytes bestehen können, wie zum Beispiel ß.

Um diese Zeichen korrekt zu behandeln, muss Gleam jeden einzelnen Buchstaben überprüfen und, falls nötig, in Kleinbuchstaben umwandeln. Das kann bei langen Zeichenfolgen zeitaufwändig sein. Wenn Performance ein wichtiger Aspekt ist, sollten wir uns daher alternative Lösungen ansehen.

Eine solche Alternative wäre die Verwendung von Unicode-Bibliotheken, um Zeichenfolgen in Kleinbuchstaben zu konvertieren. Diese Bibliotheken sind speziell für die Behandlung von Unicode-Charakteren optimiert und können die Konvertierung möglicherweise schneller durchführen.

## Siehe auch

- [Die offizielle Gleam-Dokumentation](https://gleam.run/documentation/)
- [Die Unicode-Bibliothek für Gleam](https://github.com/gleam-lang/unicode)