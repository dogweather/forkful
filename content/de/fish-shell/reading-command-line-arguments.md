---
title:    "Fish Shell: Lesen von Befehlszeilenargumenten."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Befehlszeilenargumenten kann die Effizienz und Flexibilität eines Programms erheblich verbessern. Durch das Lesen von Befehlszeilenargumenten können verschiedene Eingabeparameter gezielt angesprochen werden, ohne dass der Programmcode jedes Mal angepasst werden muss.

## Wie geht das?

Um Befehlszeilenargumente in Fish Shell zu lesen, können Sie die integrierten Variablen $argv und $argc verwenden. $argv enthält alle eingegebenen Argumente als Array, während $argc die Anzahl der Argumente angibt.

Ein Beispiel: Nehmen wir an, Sie haben ein Programm, das eine Datei mit einer bestimmten Größe erstellen soll. Sie möchten jedoch, dass die Größe der Datei durch ein Befehlszeilenargument bestimmt werden kann. In Fish Shell könnten Sie dies folgendermaßen realisieren:

```Fish Shell
if test (count $argv) -eq 2
    set dateiname $argv[1]
    set size $argv[2]
    fallocate -l $size $dateiname
else
    echo "Bitte geben Sie einen Dateinamen und eine Größe an."
end
```

Dieses Beispiel überprüft zuerst die Anzahl der eingegebenen Argumente und weist dann die Werte von $argv zu entsprechenden Variablen zu. Anschließend wird die Datei mit der gewünschten Größe erstellt. Wenn nicht genügend Argumente eingegeben wurden, wird eine Fehlermeldung ausgegeben.

## Tiefere Einblicke

Neben den integrierten Variablen gibt es in Fish Shell auch die Möglichkeit, Befehlszeilenargumente mithilfe der "argparse" Bibliothek auszulesen. Diese bietet mehr Funktionalitäten wie z.B. das Festlegen von Argumenttypen oder das Hinzufügen von Standardwerten. Weitere Informationen zur Verwendung von "argparse" finden Sie in der offiziellen Dokumentation.

## Siehe auch

- https://fishshell.com/docs/current/cmds/set.html#$argv
- https://fishshell.com/docs/current/cmds/set.html#$argc
- https://fishshell.com/docs/current/cmds/fallocate.html
- https://github.com/daniel-san/fish-argparse