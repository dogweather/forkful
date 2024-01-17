---
title:                "Umwandlung eines Datums in eine Zeichenfolge"
html_title:           "PHP: Umwandlung eines Datums in eine Zeichenfolge"
simple_title:         "Umwandlung eines Datums in eine Zeichenfolge"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Umwandeln von einem Datum in eine Zeichenfolge bedeutet, dass wir ein Datum, das in einem bestimmten Format vorliegt, in eine lesbarere und benutzerfreundlichere Form bringen. Programmierer nutzen diese Funktion, um beispielsweise das Datum in einem leserlichen Format auf einer Webseite anzuzeigen.

# Wie geht das?

Um ein Datum in eine Zeichenfolge umzuwandeln, können wir in PHP die Funktion `date()` verwenden. Hier ist ein Beispielcode, der das aktuelle Datum in dem Format "d.m.Y" (Tag.Monat.Jahr) ausgibt:

```PHP
$currentDate = date("d.m.Y");
echo $currentDate;
// Output: 13.08.2021
```

Wir können auch die Zeit hinzufügen, indem wir ein zusätzliches Argument an die Funktion übergeben. Zum Beispiel können wir das aktuelle Datum und die Zeit im Format "d.m.Y, H:i:s" (Tag.Monat.Jahr, Stunden:Minuten:Sekunden) ausgeben lassen:

```PHP
$currentDateTime = date("d.m.Y, H:i:s");
echo $currentDateTime;
// Output: 13.08.2021, 15:23:06
```

# Tiefer tauchen

Die Funktion `date()` wurde bereits in der ersten Version von PHP eingeführt und ist seitdem eine der am häufigsten genutzten Funktionen. Es gibt auch alternative Wege, ein Datum in eine Zeichenfolge umzuwandeln, wie zum Beispiel die Funktion `strftime()`, welche zusätzliche Formatierungsoptionen bietet.

Die Implementierung der Funktion `date()` basiert auf den Unix Timestamp, welcher die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970 darstellt. Dieses Datum wurde von Unix als Basis für Datumsangaben gewählt und hat sich seitdem in der Programmierwelt etabliert.

# Weitere Informationen

Weitere Informationen zu Datumsumwandlungen in PHP findest du in der offiziellen Dokumentation: https://www.php.net/manual/de/function.date.php

Für eine detailliertere Erklärung des Unix Timestamps und seiner Verwendung kannst du diesen Artikel lesen: https://de.wikipedia.org/wiki/Unixzeit

# See Also

- Offizielle Dokumentation zu `date()`: https://www.php.net/manual/de/function.date.php
- Alternative Funktion `strftime()`: https://www.php.net/manual/de/function.strftime.php
- Geschichte des Unix Timestamps: https://de.wikipedia.org/wiki/Unixzeit