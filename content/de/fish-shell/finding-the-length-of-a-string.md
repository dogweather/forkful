---
title:    "Fish Shell: Die Länge eines String finden"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

#
## Warum

Möglicherweise möchten Sie die Länge einer Zeichenfolge in Ihrem Fish Shell-Programm ermitteln, um beispielsweise die Eingabe von Benutzern zu überprüfen oder bestimmte Manipulationen an einer Zeichenfolge durchzuführen.

## So geht's

Um die Länge einer Zeichenfolge in Fish Shell zu finden, können Sie die eingebaute `count`-Funktion verwenden. Hier ist ein Beispielcode:

```Fish Shell
set string "Hallo Welt"
echo (count $string)
```

Dieses Beispiel definiert eine Variable `string` mit dem Wert "Hallo Welt" und gibt dann die Länge der Zeichenfolge aus, die 11 beträgt. Beachten Sie, dass die `count`-Funktion Leerzeichen und andere Zeichen berücksichtigt, daher ist die Länge immer die Anzahl der Zeichen in der Zeichenfolge.

## Tiefere Einblicke

Wenn Sie wirklich an der Implementierung der `count`-Funktion interessiert sind, können Sie einen Blick auf die Fish Shell-Dokumentation werfen. Die `count`-Funktion verwendet die `string length`-Funktion im Hintergrund, die wiederum die `wc`-Befehlszeile verwendet, um die Zeilenanzahl einer Datei zu zählen. Aufgrund der Funktionsweise dieser Funktion ist sie jedoch möglicherweise nicht die effizienteste Methode, um die Länge einer Zeichenfolge zu bestimmen.

## Siehe auch

- Fish Shell-Dokumentation über die `count`-Funktion: https://fishshell.com/docs/current/cmds/count.html
- Fish Shell-Dokumentation über die `string length`-Funktion: https://fishshell.com/docs/current/cmds/string_length.html
- GNU `wc`-Dokumentation: https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html