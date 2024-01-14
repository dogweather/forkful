---
title:    "Fish Shell: Die Länge eines Strings finden."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Der Benchmark-Code für die Länge einer Zeichenkette ist ein häufiges Problem bei der Arbeit mit Fish Shell. Wenn Sie jedoch noch neu in der Shell-Programmierung sind, ist es wichtig zu wissen, wie man die Länge einer Zeichenkette effizient findet. In diesem Blog-Beitrag werden wir uns mit verschiedenen Methoden beschäftigen, um die Länge einer Zeichenkette mit Fish Shell zu finden.

## Wie man die Länge einer Zeichenkette findet
Um die Länge einer Zeichenkette in Fish Shell zu finden, gibt es einige nützliche Funktionen und Befehle, die wir nutzen können. Hier sind einige Beispiele, die Ihnen dabei helfen werden, die Länge einer Zeichenkette zu bestimmen:

```
# Länge einer Zeichenkette mit dem Befehl `string length` finden
$ string length "Hallo, Welt!"
11

# Länge einer Zeichenkette mit dem Befehl `expr length` finden
$ expr length "Hallo, Welt!"
11

# Länge einer Zeichenkette mit der Funktion `string len` finden
$ string len "Hallo, Welt!"
11
```

Sie können auch die Ausgabe eines Befehls in eine Variable speichern und die Variable dann nutzen, um die Länge einer Zeichenkette zu finden:

```
# Ausgabe des Befehls `ls` in der Variable `files` speichern
$ set files (ls)

# Länge der Variable `files` finden
$ string length $files
10
```

Das sind nur einige der vielen Möglichkeiten, die Fish Shell bietet, um die Länge einer Zeichenkette zu finden. Es ist wichtig zu beachten, dass diese Methoden je nach den Anforderungen Ihres Codes unterschiedlich effizient sein können. Machen Sie daher immer einen Benchmark-Test, um die schnellste Methode für Ihren spezifischen Fall zu finden.

## Tiefere Einblicke
Bei der Suche nach der Länge einer Zeichenkette gibt es noch andere Faktoren zu beachten, die die Leistung beeinflussen können. Zum Beispiel kann die Verwendung von Leerzeichen oder Sonderzeichen in der Zeichenkette die Länge verändern. Dies kann insbesondere beim Einsatz von Wildcards wie `*` und `?` problematisch werden.

Eine Möglichkeit, die Länge einer Zeichenkette zu berechnen, die auch Leerzeichen und Sonderzeichen berücksichtigt, ist die Verwendung von `echo -n` zusammen mit `wc`:

```
# Länge einer Zeichenkette mit `echo -n` und `wc` finden
$ echo -n "Hallo, Welt!" | wc -c
12
```

Dies funktioniert, indem `echo -n` die Zeichenkette ohne ein abschließendes Zeilenumbruchzeichen ausgibt und `wc -c` die Anzahl der angezeigten Zeichen zählt.

Insgesamt gibt es viele verschiedene Methoden, um die Länge einer Zeichenkette in Fish Shell zu finden. Experimentieren Sie mit verschiedenen Methoden und nutzen Sie Benchmarking, um die effizienteste Methode für Ihren Code zu finden.

## Siehe auch

- [Fish Shell-Dokumentation für die `string`-Funktionen](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell-Dokumentation für die `expr`-Funktion](https://fishshell.com/docs/current/cmds/expr.html)
- [Fish Shell-Dokumentation für die `set`-Funktion](https://fishshell.com/docs/current/cmds/set.html)
- [Fish Shell-Dokumentation für `echo` und `wc`](https://fishshell.com/docs/current/cmds/echo.html)