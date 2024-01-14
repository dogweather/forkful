---
title:    "Fish Shell: Debug-Ausgabe drucken"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum
Warum sollte man debug Output in seinem Fish Shell Programm ausgeben? Es gibt mehrere Gründe, warum dies eine nützliche Praxis ist. Einerseits kann es dabei helfen, Fehler in deinem Code zu finden und zu beheben. Andererseits kann es auch dazu dienen, Informationen über den Programmablauf zu erhalten und somit bei der Entwicklung zu unterstützen.

# Wie geht man vor
Um debug Output in Fish Shell zu drucken, gibt es verschiedene Möglichkeiten. Eine davon ist die `echo` Funktion, die standardmäßig in Fish Shell verfügbar ist. Hier ein Beispiel, wie man sie benutzen kann:

```
Fish Shell Beispiel
echo "Dies ist ein debug Output"
```

Dieses einfache Beispiel gibt den Text "Dies ist ein debug Output" aus. Mit dieser Methode ist es möglich, verschiedene Variablen und Daten auszugeben, um den Programmablauf besser zu verstehen.

Weitere Möglichkeiten um debug Output in Fish Shell zu drucken, sind die Verwendung von `printf` oder `debug` Befehlen. Ein Beispiel für `printf` wäre:

```
Fish Shell Beispiel
set name "Peter"
printf "Der Name ist: %s" $name
```

Dieser Code würde den debug Output "Der Name ist: Peter" ausgeben. Als Alternative gibt es auch den `debug` Befehl, der speziell für Debugging Zwecke entwickelt wurde. Hier ein Beispiel:

```
Fish Shell Beispiel
debug "Dies ist ein debug Output"
```

Dieser Befehl gibt nicht nur die Nachricht aus, sondern auch zusätzliche Informationen wie die Zeile und die Datei, in der der Befehl ausgeführt wurde.

# Deep Dive
Wenn es darum geht, tiefere Informationen über den Programmablauf zu erhalten, ist es hilfreich, den `psub` Befehl zu verwenden. Dieser Befehl zeigt alle Funktionen an, die während der Ausführung des Codes aufgerufen wurden. Hier ein Beispiel:

```
Fish Shell Beispiel
function hello
  echo "Hallo"
end

hello
psub
```

Die Ausgabe dieses Codes würde Folgendes anzeigen:

```
Hallo
(hello)
```

Dies bedeutet, dass die Funktion "hello" aufgerufen wurde und der debug Output "Hallo" gedruckt wurde. Auf diese Weise kann man den genauen Ablauf des Codes nachvollziehen und eventuelle Fehler leichter finden.

# Siehe auch
- [Fish Shell Dokumentation über Debugging](https://fishshell.com/docs/current/commands.html#debug)
- [Beispiel Code für das Drucken von debug Output in Fish Shell](https://github.com/fish-shell/fish-shell/blob/master/test/functions/debug.test)
- [Tutorial zur Verwendung von `psub` in Fish Shell](https://medium.com/@yutang/psub-function-call-trace-in-fish-shell-debugging-af3577085be9)