---
title:    "Gleam: Das aktuelle Datum erhalten"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Die heutige Welt wird immer digitaler und technologieabhängiger. Um in dieser Umgebung effektiv zu programmieren, ist es wichtig, die aktuelle Zeit und Datum zu ermitteln. Das kann hilfreich sein, um beispielsweise zeitabhängige Aktionen auszuführen oder verwendete Ressourcen zu verfolgen.

## So geht's

Um das aktuelle Datum in Gleam zu erhalten, können wir die integrierte ```date``` Funktion verwenden. Diese Funktion gibt ein Modul zurück, das verschiedene Methoden zum Umgang mit Datumsangaben enthält. Hier ist ein einfaches Beispiel:

```Gleam
let date = date.now()
io.println(date.date())
```

Dieses Code-Beispiel gibt das aktuelle Datum im Format "yyyy-mm-dd" aus. Es gibt auch Möglichkeiten, die Uhrzeit oder bestimmte Teile des Datums zurückzugeben. Ein tieferer Einblick in die verschiedenen Methoden, die das ```date``` Modul bietet, wird im nächsten Abschnitt gegeben.

## Tiefergehende Informationen

Das ```date``` Modul erlaubt es uns, das aktuelle Datum detailliert zu untersuchen und zu formatieren. Es beinhaltet Funktionen zum Abrufen von Wochentagen, Monaten und Jahren, sowie für die Berechnung von Differenzen zwischen verschiedenen Datumsangaben. Außerdem erlaubt es uns, die Darstellung des Datums in verschiedenen Formaten zu definieren. Zum Beispiel können wir das Datum als "dd Monat yyyy" oder als "Monat dd, yyyy" anzeigen lassen.

Hier sind einige weitere Beispiele für die Verwendung des ```date``` Moduls:

- ```date.dayOfWeek(date)```: Gibt den Wochentag einer bestimmten Datumsangabe zurück, z.B. "Samstag"
- ```date.year(date)```: Gibt die Jahreszahl einer Datumsangabe zurück
- ```date.diff(date1, date2)```: Berechnet die Differenz zwischen zwei Datumsangaben in Tagen, Monaten oder Jahren

Diese sind nur einige Beispiele, aber das ```date``` Modul bietet noch viele weitere nützliche Funktionen. Für mehr Informationen können Sie die offizielle [Dokumentation](https://gleam.run/docs/standard-library/date/) zu diesem Modul konsultieren.

## Siehe auch

- [Offizielle Dokumentation des date Moduls](https://gleam.run/docs/standard-library/date/)
- [Gleam Blog: Using the date Module](https://gleam.run/blog/using-the-date-module/) - Ein weiteres hilfreiches Beispiel für die Verwendung des ```date``` Moduls in Gleam.
- [Coderontheroad: Date Manipulation in Gleam](https://coderontheroad.com/2020/07/21/date-manipulation-in-gleam/) - Ein ausführlicher Artikel, der sich mit verschiedenen Arten der Datummanipulation in Gleam beschäftigt.