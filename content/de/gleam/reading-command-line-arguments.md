---
title:    "Gleam: Lesen von Befehlszeilenargumenten"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Warum

Bevor wir uns mit dem Lesen von Befehlszeilenargumenten in Gleam beschäftigen, stellt sich vielleicht die Frage, warum man das überhaupt tun sollte. Die Antwort ist einfach: Das Lesen von Befehlszeilenargumenten ermöglicht es uns, unsere Programme flexibler zu gestalten. Anstatt jedes Mal den Code ändern zu müssen, können wir verschiedene Einstellungen und Optionen über die Befehlszeile an unser Programm übergeben.

# Wie geht man vor

Um Befehlszeilenargumente in Gleam zu lesen, können wir die `gleam/io` Bibliothek verwenden. Dort gibt es eine Funktion namens `Arg.parse`, die alle übergebenen Argumente in eine Liste umwandelt. Schauen wir uns das mal an:

```Gleam
import gleam/io as io

fn main() {
  let args = io.Arg.parse()
  io.print(args)
}
```

Wenn wir dieses Programm nun mit dem Befehl `gleam run file.gleam arg1 arg2` ausführen, wird in der Konsole `["arg1", "arg2"]` ausgegeben. Wie du siehst, werden alle Argumente in einer Liste gespeichert, die wir dann beliebig verwenden können. Zum Beispiel könnten wir ein Programm schreiben, das abhängig von den übergebenen Argumenten unterschiedliche Aktionen ausführt.

Um die Argumente genauer zu untersuchen, können wir auch die `Arg.parse_with` Funktion verwenden, die es uns erlaubt, bestimmten Argumenten spezifische Typen zuzuweisen. Mehr darüber erfährst du in unserem nächsten Teil: Deep Dive.

# Tiefer in die Materie eintauchen

Das Lesen von Befehlszeilenargumenten kann sehr nützlich sein, besonders wenn wir unsere Programme anpassungsfähiger machen wollen. Aber es gibt auch einige Dinge, auf die wir achten müssen. Zum Beispiel müssen wir sicherstellen, dass die übergebenen Argumente in der richtigen Reihenfolge stehen und dass wir die richtigen Typen zugewiesen haben.

In Gleam können wir auch Optionen mit Befehlszeilenargumenten verwenden. Dabei müssen wir jedoch darauf achten, dass bestimmte Argumente nur einmal vorkommen dürfen und andere mehrmals. Zum Beispiel könnte eine Option `-f` nur einmal verwendet werden, um eine Datei zu öffnen, während `-v` mehrmals verwendet werden könnte, um die Ausgabe zu erweitern.

Weitere Informationen und Beispiele findest du in der offiziellen Dokumentation zu Befehlszeilenargumenten in Gleam.

# Siehe auch

- [Offizielle Dokumentation zur `gleam/io` Bibliothek](https://gleam.run/articles/io)
- [Beispielprogramm für die Verwendung von Befehlszeilenargumenten in Gleam](https://github.com/user/example)