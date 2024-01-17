---
title:                "Die Länge eines Strings finden"
html_title:           "Bash: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Finden der Länge einer Zeichenfolge ist ein häufiges Problem für Programmiererinnen und Programmierer. Grund dafür ist, dass die Länge einer Zeichenfolge für verschiedene Anwendungen relevant sein kann, wie z.B. das Überprüfen von Input-Limits oder das Formatieren von Ausgaben.

## Wie geht's?

Um die Länge einer Zeichenfolge in Bash zu finden, können wir das integrierte `expr` Tool verwenden. Wir geben einfach die Zeichenfolge innerhalb von Anführungszeichen in den `expr length` Befehl ein und erhalten die Länge als Ausgabe.

```Bash
string="Hallo Welt!"
length=`expr length "$string"`
echo "Die Länge der Zeichenfolge ist: $length"
```

Die Ausgabe lautet: `Die Länge der Zeichenfolge ist: 11`

## In die Tiefe gehen

Die `expr length` Funktion ist Teil des GNU Core Utilities Pakets und wurde 1987 von Brian Kernighan und Dennis Ritchie entwickelt. Eine alternative Möglichkeit, die Länge einer Zeichenfolge in Bash zu finden, ist die Verwendung von `wc -c`, welches die Anzahl der Zeichen in einer Datei oder einem Input zählt.

Eine interessante Tatsache ist, dass es auch möglich ist, die Länge einer Zeichenfolge ohne externe Tools oder Funktionen zu finden. Dies kann erreicht werden, indem man die String-Length-Expansion von Bash verwendet: `${#string}`.

## Siehe auch

- [GNU Core Utilities](https://www.gnu.org/software/coreutils/)
- [Bash String Manipulation](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)