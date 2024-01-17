---
title:                "Umwandlung eines Datums in einen String"
html_title:           "Bash: Umwandlung eines Datums in einen String"
simple_title:         "Umwandlung eines Datums in einen String"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Konvertieren eines Datums in eine Zeichenfolge ist ein häufiges Problem bei der Programmierung. Dabei geht es darum, ein Datum, das möglicherweise in einer variablen Form gespeichert ist, in eine lesbare und einheitliche Zeichenfolge umzuwandeln. Programmierer verwenden dies oft, um Daten in einer bestimmten Formatierung auszugeben oder um sie für bestimmte Berechnungen zu verwenden.

# Wie geht das?
```Bash
date '+%d-%m-%y'
```
Dieser Befehl verwendet die `date`-Funktion zusammen mit der Formatierungsoption `+%d-%m-%y`, um das aktuelle Datum im Format Tag-Monat-Jahr auszugeben.

Die Ausgabe sieht beispielsweise so aus: `24-08-21`

```Bash
date -d "1 month" '+%d-%m-%y'
```
Hier wird das Datum für einen Monat in der Zukunft ausgegeben, indem der Parameter `+1 month` verwendet wird. Die Ausgabe sieht ähnlich aus wie im vorherigen Beispiel, mit dem Unterschied, dass der Monat um eins erhöht wurde.

# Tief einsteigen
Die Idee, ein Datum in eine lesbare Zeichenfolge umzuwandeln, ist nicht neu. Seit den Anfängen der Programmierung haben Programmierer immer neue Methoden zur Formatierung von Datum und Zeit erfunden. Heutzutage gibt es viele Alternativen zum `date`-Befehl, wie zum Beispiel Bibliotheken wie `strftime` in C oder Funktionen in anderen Programmiersprachen. Die Implementierung dieses Befehls basiert auf der seit langem bewährten `C time library` und wird mit jeder neuen Version von Bash ständig verbessert.

# Siehe auch
- [Bash-Dokumentation für `date` Befehl](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Wikipedia-Artikel zur Zeitdarstellung und -berechnung](https://en.wikipedia.org/wiki/Date_and_time_notation_by_country)