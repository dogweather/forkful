---
title:                "Gleam: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum 

Es gibt viele Gründe, warum jemand eine Programmiersprache wie Gleam nutzen würde. Einer davon ist die Fähigkeit, Strings in Kleinbuchstaben zu konvertieren. Dies ist besonders nützlich, wenn Sie die Eingabe von Benutzern standardisieren oder einfach nur Strings in Ihrem Code vergleichen möchten.

## Wie geht das?

Die Konvertierung eines Strings in Kleinbuchstaben ist in Gleam recht einfach. Zunächst müssen Sie jedoch sicherstellen, dass Sie die "string" Bibliothek importieren, da diese die Funktionen enthält, die wir benötigen.

```Gleam
import gleam/string

// Eingabe String
let name = "Silvia"

// Konvertierung zu Kleinbuchstaben
let lower_case_name = string.to_lower(name)

// Ausgabe des konvertierten Strings "silvia"
```

In unserem Beispiel haben wir den Namen "Silvia" eingegeben und mit der "to_lower" Funktion in kleinere Buchstaben umgewandelt. Die Ausgabe ist der konvertierte String "silvia".

## Tief in die Materie eintauchen

Um besser zu verstehen, wie die Konvertierung eines Strings in Kleinbuchstaben funktioniert, können wir uns den Quellcode der "string" Bibliothek ansehen. Hier finden wir die "to_lower" Funktion, die den gegebenen String in eine Liste von einzelnen Buchstaben zerlegt und dann jeden davon in einen Kleinbuchstaben umwandelt, bevor sie wieder zu einem String zusammengesetzt werden.

Wir können auch ein wenig über die Effizienz und Komplexität der Funktion lernen. Zum Beispiel sehen wir, dass es einen zusätzlichen Parameter gibt, um eine benutzerdefinierte Locale für die Konvertierung anzugeben. Dies ermöglicht es uns, Strings in verschiedene Sprachen zu konvertieren, die möglicherweise unterschiedliche Regeln für die Kleinbuchstabenumwandlung haben.

## Siehe auch

* [Offizielle Gleam Dokumentation](https://gleam.run/book/introduction.html)
* [String Bibliothek Referenz](https://gleam.run/modules/gleam_std/string.html)
* [Praktische Gleam Beispiele](https://github.com/gleam-lang/gleam/tree/master/examples)