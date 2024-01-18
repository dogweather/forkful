---
title:                "Ein Datum aus einem String auslesen"
html_title:           "Fish Shell: Ein Datum aus einem String auslesen"
simple_title:         "Ein Datum aus einem String auslesen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Parsen eines Datums aus einem String bedeutet, ein Datum aus einer Zeichenkette zu extrahieren und in ein Standardformat umzuwandeln. Programmierer tun dies, um mit Datumswerten arbeiten zu können, wie zum Beispiel beim Vergleichen oder Sortieren. 

# Wie geht's?

```
# Ein Beispiel mit Fish Shell

set str "13.12.2020"
set format "+%Y-%m-%d"
echo (date -f $format $str)

# Ausgabe: 2020-12-13
```

Das obige Beispiel zeigt, wie man mit Fish Shell ein Datum aus einem String extrahieren kann. Zuerst wird die Zeichenkette "13.12.2020" in der Variable str gespeichert. Dann wird das Format "+%Y-%m-%d" definiert, das dem Standardformat für Datumswerte entspricht. Schließlich wird mit dem Befehl "date" das Datum aus dem String in das definierte Format umgewandelt und ausgegeben.

# Tief Einblick

Das Parsen von Datumswerten aus Zeichenketten ist in der Programmierung sehr wichtig, um mit Datumswerten arbeiten zu können. Früher war dies oft eine zeitaufwändige Aufgabe, da unterschiedliche Länder verschiedene Datumsformate verwenden. In Fish Shell wird dies jedoch durch die Verwendung des Befehls "date" erleichtert, der eine Vielzahl von Optionen bietet, um Datumswerte zu konvertieren und zu formatieren.

Alternativen zu Fish Shell für das Parsen von Datumswerten sind zum Beispiel die Programmiersprachen Python oder Java. Auch hier werden spezielle Funktionen für das Extrahieren von Datumswerten aus Zeichenketten angeboten. In Fish Shell ist es jedoch besonders einfach, da der Befehl "date" bereits vorhanden ist und die Arbeit erleichtert.

# Siehe auch

- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- Ein Tutorial zum Parsen von Datumswerten aus Zeichenketten mit Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_parsing