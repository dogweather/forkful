---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Bash-Programmierung: Ein Datum in einen String umwandeln
Heute werden wir uns ansehen, wie wir ein Datum in Bash in einen Zeichenkette (String) umwandeln und warum das überhaupt wichtig ist.

## Was & Warum?
Ein Datum in einen String zu konvertieren bedeutet im Grunde, das Datum in eine lesbare Form zu bringen. Warum machen wir das? Weil wir oft die Ausgabe von Datum und Zeit formatieren müssen, um sie für bestimmte Aufgaben, wie zum Beispiel Berichterstattung und Protokollierung, geeignet zu machen.

## So geht's:

In Bash können wir das `date` Befehl verwenden, um ein Datum zu erzeugen und es mit Hilfe von Formatierungsoptionen in eine Zeichenkette umzuwandeln. Hier sind einige Beispiele:

```Bash
# Aktuelles Datum in einem String
datum=$(date +"%d-%m-%Y")
echo $datum
```
Ausgabe:
```Bash
01-07-2021
```

```Bash
# Aktuelle Uhrzeit in einem String
zeit=$(date +"%H:%M:%S")
echo $zeit
```
Ausgabe:
```bash
14:30:00
```

Die Formatierungsoptionen sind sehr flexibel, und du kannst sie nach deinen Anforderungen anpassen.

## Vertiefung:

Historisch gesehen ist das `date` Befehl eine der ältesten Methoden in Unix und Linux, um Datum und Zeit zu manipulieren und anzuzeigen. Es bietet viele Flexibilität und daher ist es bis heute in Gebrauch.

Alternative Methoden zur Datums- und Zeitformatierung in der Bash sind die Nutzung von eingebauten Funktionen in Programmiersprachen wie awk oder perl. Diese können mehr Leistung bieten, erfordern jedoch ein tieferes Verständnis der jeweiligen Sprache.

Die Umwandlung eines Datums in einen String in Bash geschieht im Wesentlichen durch die Formatierung der Ausgabe des `date` Befehls. Mit verschiedenen Platzhaltern kannst du bestimmen, welche Teile des Datums und in welchem Format angezeigt werden.

## Siehe auch:

Für eine detailliertere Anleitung zur Formatierung von Datum und Zeit in Bash, siehe: 
- [Bash by example - Date and Time](https://www.ibm.com/developerworks/library/l-bash2/index.html)

Für Alternativen zur Datumsformatierung in awk oder perl, siehe:
- [Date Manipulation in Awk](http://www.awk.info/?date)