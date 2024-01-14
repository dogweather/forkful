---
title:    "Bash: Debug-Ausgabe drucken"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum
Debug-Ausgaben sind ein wesentlicher Bestandteil der Entwicklung von Bash-Skripten. Sie ermöglichen es uns, Fehler zu identifizieren und zu beheben, indem sie uns Einblicke in die Ausführung unseres Codes geben. Ohne Debug-Ausgaben wäre es viel schwieriger, den Grund für einen Fehler zu erkennen und zu beheben.

## Wie geht das?
Das Hinzufügen von Debug-Ausgaben in unser Bash-Skript ist einfach und hilfreich. Wir können das Befehl `echo` verwenden, um gezielte Nachrichten auszugeben, während das Skript ausgeführt wird. Beispiel:

```Bash
#!/bin/bash
echo "Das Skript wird jetzt ausgeführt."
n=$1
echo "Der Wert von n ist $n."
if [ $n -gt 10 ]
then
  echo "Der Wert von n ist größer als 10."
else
  echo "Der Wert von n ist kleiner oder gleich 10."
fi
```

Dieses einfache Skript gibt Ausgaben aus, die uns helfen, die Ausführung besser zu verstehen. Indem wir den Wert von `n` ausgeben, können wir sicherstellen, dass er korrekt eingelesen wurde. Außerdem gibt die Ausgabe innerhalb der `if`-Bedingung uns die Gewissheit, dass die richtige Anweisung ausgeführt wurde.

## Deep Dive
Debug-Ausgaben sollten strategisch platziert werden, um die Effizienz zu maximieren. Zu viele Ausgaben können den Code unübersichtlich machen und zu Verwirrung führen. Aus diesem Grund sollten wir sorgfältig überlegen, welche Variablen und Nachrichten wir ausgeben möchten.

Eine weitere hilfreiche Methode ist die Verwendung des `set -x`-Befehls am Anfang des Skripts. Dadurch werden alle Befehle und Variablenausgaben angezeigt, während das Skript ausgeführt wird. Beispiel:

```Bash
#!/bin/bash
set -x
echo "Das Skript wird jetzt ausgeführt."
n=$1
echo "Der Wert von n ist $n."
if [ $n -gt 10 ]
then
  echo "Der Wert von n ist größer als 10."
else
  echo "Der Wert von n ist kleiner oder gleich 10."
fi
```

Die Ausgabe dieses Skripts zeigt uns alle von Bash ausgeführten Befehle sowie die zugewiesenen Werte an.

## Siehe auch
- [Bash-Befehle echo und read](https://www.linux-praxis.de/lpi/d-bash-kommandozeile.php)
- [Debugging mit Bash](https://wiki.ubuntuusers.de/Bash/Debugger/)
- [Einführung in die Shell-Programmierung mit Bash](https://openbook.rheinwerk-verlag.de/shell_programmierung/shell_007_000.htm)