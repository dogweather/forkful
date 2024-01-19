---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Generierung von Zufallszahlen ist ein Verfahren, mit dem nicht vorhersehbare Zahlen erzeugt werden. Programmierer verwenden dies häufig für Anwendungen in Kryptographie, Simulationen, Spielen und mehr.

## So geht's:
Mit Bash können Sie ganz leicht Zufallszahlen generieren. Hier sind einige Beispiele:

```bash
# Generiere eine zufällige Zahl
echo $RANDOM
```

Ausgabe könnte so etwas sein:

```bash
28421
```

Wenn Sie eine zufällige Zahl zwischen 1 und 100 wollen, können Sie den Modulo Operator (%) verwenden:

```bash
echo $((RANDOM%100+1))
```

Ausgabe könnte sein:

```bash
42
```

## Deep Dive:

Die Bash-Funktion $RANDOM existiert bereits seit vielen Jahren und ist eine eingebaute Funktion der Bash Shell. Sie liefert Zahlen im Bereich 0 bis 32767.

Alternativ können Sie auch `openssl` oder `shuf` verwenden, um Zufallszahlen zu generieren, abhängig von Ihren spezifischen Anforderungen. Mit `shuf` können Sie zum Beispiel leicht eine zufällige Reihenfolge einer Liste von Elementen erzeugen.

Einige mögen denken, dass die Nutzung von `$RANDOM` begrenzt ist aufgrund seiner maximalen Größe, das muss jedoch nicht der Fall sein. Sie können mehrere Male `$RANDOM` in Kombination nutzen, um größere Zahlen zu generieren.

Es gibt viele Methoden, um Zufallszahlen in Bash zu erzeugen, aber die Wahl der Methode hängt von dem spezifischen Anwendungsfall oder von der benötigten Zufallsgüte ab.

## Siehe auch:

Für mehr Informationen, folgenden Sie bitte diesen Links:

1. Das Bash Handbuch - https://www.gnu.org/software/bash/manual/
2. OpenSSL Dokumentation - https://www.openssl.org/docs/
3. Shuf Mann-Seite - https://linux.die.net/man/1/shuf