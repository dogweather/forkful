---
title:    "Fish Shell: Vergleich zweier Daten"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist ein wichtiger Teil des Programmierens, besonders wenn es um die Verarbeitung von Datum und Zeit geht. Mit dem Fish Shell gibt es eine schnelle und einfache Möglichkeit, zwei Daten miteinander zu vergleichen.

## Wie geht's

Um zwei Daten im Fish Shell zu vergleichen, verwenden wir den `date`-Befehl. Dieser Befehl gibt das aktuelle Datum aus, das standardmäßig im Format "Wochentag Monat Tag Stunden:Minuten:Sekunden Zeitzone Jahr" angegeben ist.

Für dieses Beispiel erstellen wir zwei Datumswerte:

```
Fish Shell $ set start (date +%s)
Fish Shell $ set end (date -u +%s)
```
Der Befehl `date +%s` gibt das Datum im Unix-Zeitformat aus, das die Anzahl der seit dem 1. Januar 1970 vergangenen Sekunden angibt. Durch Zuweisen dieser Werte zu den Variablen `start` und `end` haben wir nun zwei vergleichbare Datumswerte.

Um diese beiden Daten zu vergleichen, verwenden wir den `test`-Befehl mit der Option `-gt`, die für "größer als" steht. Das sieht folgendermaßen aus:

```
Fish Shell $ if [ $start -gt $end ]
            echo "Das Startdatum ist später als das Enddatum."
          else
            echo "Das Startdatum ist früher als das Enddatum."
          end
```

Die Ausgabe wird je nach Zeitpunkt der Ausführung entweder "Das Startdatum ist später als das Enddatum." oder "Das Startdatum ist früher als das Enddatum." sein.

Eine weitere Möglichkeit ist die Verwendung des `seq`-Befehls, der eine Sequenz von Zahlen generiert. Wir können diesen Befehl verwenden, um einen Zeitraum von Daten zu erstellen und diese dann miteinander zu vergleichen. Hier ist ein Beispiel:

```
Fish Shell $ seq $start 100 $end | while read i; and [ $i == $end ]; and echo "Enddatum erreicht!"
```

Dieser Befehl generiert eine Sequenz von Zahlen, beginnend beim Wert des Startdatums und mit einem Schritt von 100, bis es den Wert des Enddatums erreicht. Dann wird eine simple Ausgabe "Enddatum erreicht!" ausgegeben.

## Tiefergehende Informationen

Das Vergleichen von Daten im Fish Shell hängt von der Verwendung von Shell-Variablen und Shell-Befehlen ab. Es gibt viele Möglichkeiten, diese Variablen und Befehle zu kombinieren, um verschiedene Daten zu vergleichen. Einige nützliche Befehle sind `test`, `seq`, `date`, `if` und `while`.

## Siehe auch

- [Fish Shell-Dokumentation](https://fishshell.com/docs/current/index.html)
- [Shell-Variablen](https://fishshell.com/docs/current/variables.html)
- [Shell-Befehle](https://fishshell.com/docs/current/index.html#commands)
- [Shell-Steuerstrukturen](https://fishshell.com/docs/current/index.html#scripts.flow)