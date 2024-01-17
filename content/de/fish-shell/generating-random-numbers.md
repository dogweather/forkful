---
title:                "Erzeugung von Zufallszahlen"
html_title:           "Fish Shell: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was ist das und warum machen Programmierer das?

Das Generieren von Zufallszahlen ist eine Methode, um eine zufällige Zahl innerhalb eines bestimmten Wertebereichs zu erstellen. Programmierer nutzen diese Technik oft, um zufällige Werte für Spiele, Verschlüsselung oder Tests zu erzeugen.

## Wie geht das?

Das Erstellen von Zufallszahlen ist in Fish Shell sehr einfach. Nutze einfach den Befehl `math random` und gib den gewünschten Wertebereich an. Zum Beispiel:

```
Fish Shell math random 1 10
# gibt eine zufällige Zahl zwischen 1 und 10 zurück
```

Du kannst auch den Zusatz `--count` nutzen, um eine bestimmte Anzahl von Zufallszahlen zu generieren. Zum Beispiel:

```
Fish Shell math random --count=5 1 10
# gibt 5 verschiedene Zufallszahlen zwischen 1 und 10 zurück
```

## Tiefer Einblick

Das Generieren von Zufallszahlen ist ein wichtiger Teil der Programmierung. Es wird oft benutzt, um eine zufällige Reihenfolge in Spiele, Musik-Playlists oder Lottozahlen zu erzeugen. Es gibt auch verschiedene Algorithmen, die für die Generierung von Zufallszahlen verwendet werden, jeder mit seinen eigenen Vor- und Nachteilen.

Eine Alternative zum `math random` Befehl in Fish Shell ist die Verwendung der `shuf` Funktion, die zufällige Zeilen innerhalb einer Datei ausgibt.

In Fish Shell wird der `math random` Befehl basierend auf einer XORshift-Pseudozufallszahlengenerator-Implementierung ausgeführt.

## Siehe auch

- Offizielle Dokumentation für den Fish Shell `math random` Befehl: https://fishshell.com/docs/current/commands.html#math-random
- Eine Einführung in die Bedeutung von Zufallszahlen in der Programmierung: https://de.wikipedia.org/wiki/Zufallszahlengenerator