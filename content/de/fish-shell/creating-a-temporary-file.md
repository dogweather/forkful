---
title:    "Fish Shell: Erstellen einer temporären Datei"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung kann es oft notwendig sein, temporäre Dateien zu erstellen. Diese Dateien dienen als Zwischenspeicher für Daten oder als vorübergehender Speicherplatz für bestimmte Berechnungen. In der Fish Shell gibt es verschiedene Möglichkeiten, temporäre Dateien zu erstellen, die wir uns im Folgenden genauer ansehen werden.

## Wie geht man vor?

Das Erstellen einer temporären Datei ist in der Fish Shell relativ unkompliziert. Wir nutzen dafür das integrierte Kommando `mktemp`. Folgende Befehle führen dazu, dass eine temporäre Datei erstellt und anschließend wieder gelöscht wird:

```Fish Shell
$datei = (mktemp)
echo "Hallo Welt" > $datei
rm $datei
```

Wenn wir die Befehle in der Fish Shell ausführen, sehen wir in der Ausgabe eine zufällig generierte Dateinamen, wie zum Beispiel `/tmp/tmp.IU8U3QX6YF` in unserem Beispiel.

## Tiefergehende Informationen

Die Verwendung von `mktemp` hat einige Vorteile gegenüber anderen Methoden zur Erstellung von temporären Dateien. Zum einen sind die generierten Namen zufällig, wodurch es unwahrscheinlicher wird, dass es zu Namenskonflikten kommt. Zudem stellt `mktemp` sicher, dass die Datei nur für den aktuellen Benutzer erstellbar ist, was die Sicherheit erhöht.

Eine alternative Methode zur Erstellung von temporären Dateien ist die Verwendung des Kommandos `touch`. Hierbei wird jedoch kein zufälliger Name generiert und es besteht die Möglichkeit, dass es zu Konflikten kommt, falls mehrere Benutzer gleichzeitig temporäre Dateien erstellen.

## Siehe auch

- [Fish Shell Offizielle Website](https://fishshell.com/)
- [Einführung in die Fish Shell](https://www.linux-magazin.de/ausgaben/2018/08/fish-shell-3/) (auf Deutsch)
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html) (auf Englisch)