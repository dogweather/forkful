---
title:    "Bash: Die Länge eines Strings ermitteln"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist eine grundlegende Fähigkeit in der Bash-Programmierung. Es ermöglicht Ihnen, die Größe und Struktur von Texten zu überprüfen und effektiv mit ihnen zu arbeiten. In diesem Blog-Beitrag werden wir uns ansehen, wie man die Länge einer Zeichenkette in Bash finden kann.

## Wie geht's

Um die Länge einer Zeichenkette in Bash zu finden, können wir den Befehl `expr length` verwenden. Hier ist ein Beispiel, wie man es benutzt:

```Bash
string="Hallo Welt"
echo "Die Länge der Zeichenkette ist: $(expr length $string)"
```

Die Ausgabe dieses Codes wird folgendermaßen aussehen:

```
Die Länge der Zeichenkette ist: 10
```

Der Befehl `expr length` gibt die Anzahl der Buchstaben in der Zeichenkette aus, einschließlich Leerzeichen und Sonderzeichen. Sie können ihn auch mit Variablen oder Benutzereingaben verwenden. Hier ist ein weiteres Beispiel:

```Bash
read -p "Geben Sie eine Zeichenkette ein: " string
echo "Die Länge der Zeichenkette ist: $(expr length $string)"
```

Dies wird den Benutzer auffordern, eine Zeichenkette einzugeben und dann die Länge der eingegebenen Zeichenkette ausgeben.

## Tiefer Einblick

Es gibt auch eine andere Möglichkeit, die Länge einer Zeichenkette in Bash zu finden, indem man die `#`-Operator verwendet. Hier ist ein Beispiel:

```Bash
string="Hallo Welt"
echo "Die Länge der Zeichenkette ist: ${#string}"
```

Dieses wird die gleiche Ausgabe wie das vorherige Beispiel erzeugen. Der `#`-Operator gibt die Anzahl der Zeichen in der Zeichenkette aus, ohne Leerzeichen zu berücksichtigen.

Beachten Sie, dass die Länge einer Zeichenkette in Bash immer in ganzen Zahlen angegeben wird, unabhängig von der tatsächlichen Größe der Zeichenkette. Dies kann dazu führen, dass die Ausgabe länger oder kürzer aussieht als erwartet.

## Siehe auch

- [Bash-Referenzhandbuch für Zeichenketten](https://www.gnu.org/software/bash/manual/html_node/String-Operationen.html)
- [Offizielle Dokumentation für den `expr` Befehl](https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html)
- [Eine Einführung in die Bash-Programmierung](https://www.linuxjournal.com/content/introduction-bash-programming)