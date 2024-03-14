---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:06.029488-07:00
description: "Das Berechnen eines Datums in der Zukunft oder Vergangenheit umfasst\
  \ die Bestimmung eines Datums, das eine festgelegte Anzahl von Tagen, Monaten oder\u2026"
lastmod: '2024-03-13T22:44:53.733267-06:00'
model: gpt-4-0125-preview
summary: "Das Berechnen eines Datums in der Zukunft oder Vergangenheit umfasst die\
  \ Bestimmung eines Datums, das eine festgelegte Anzahl von Tagen, Monaten oder\u2026"
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit umfasst die Bestimmung eines Datums, das eine festgelegte Anzahl von Tagen, Monaten oder Jahren von einem gegebenen Datum entfernt ist. Programmierer benötigen diese Funktionalität häufig, um Erinnerungen, Abonnements, Ablaufdaten und Planungsaufgaben in verschiedenen Anwendungen zu automatisieren.

## Wie:
In Visual Basic for Applications (VBA) ist die primäre Funktion, die verwendet wird, um zukünftige oder vergangene Termine zu berechnen, `DateAdd()`. Diese Funktion fügt einem Datum ein bestimmtes Zeitintervall hinzu und gibt ein neues Datum zurück.

Hier ein einfaches Beispiel, um 10 Tage zum aktuellen Datum hinzuzufügen:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' Fügt dem aktuellen Datum 10 Tage hinzu
Debug.Print futureDate ' Gibt etwas ähnliches aus: 20.04.2023
```

Ähnlich, um ein Datum zu finden, das 10 Tage in der Vergangenheit liegt:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' Subtrahiert 10 Tage vom aktuellen Datum
Debug.Print pastDate ' Gibt aus: 31.03.2023, wenn heute der 10.04.2023 ist
```

Diese Beispiele sind ziemlich einfach. Sie können `"d"` durch andere Intervallcodes ersetzen, wie `"m"` für Monate und `"yyyy"` für Jahre, um verschiedene Arten von Datumsberechnungen durchzuführen. Hier ist ein Beispiel, wie Sie ein Datum ein Jahr in der Zukunft berechnen könnten:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' Fügt dem aktuellen Datum 1 Jahr hinzu
Debug.Print nextYear ' Gibt aus: 10.04.2024, wenn heute der 10.04.2023 ist
```

## Tiefergehende Betrachtung
Die Funktion `DateAdd` ist seit ihrer Einführung ein grundlegender Teil von VBA, abgeleitet von ihrem Vorgänger BASIC. Obwohl sie Einfachheit für das Hinzufügen oder Subtrahieren von Zeitintervallen zu Daten bietet, ist es wichtig zu beachten, dass VBA, einschließlich seiner Datumsbehandlungsfunktionen, möglicherweise nicht immer den Komfort oder die Effizienz bietet, die in neueren Programmiersprachen zu finden sind.

Moderne Sprachen wie Python mit dem Modul `datetime` oder JavaScript mit Bibliotheken wie `moment.js` und `date-fns` bieten intuitivere und leistungsfähigere Möglichkeiten zur Datumsmanipulation. Diese Optionen bieten eine bessere Unterstützung für Lokalisierung, Zeitzonen und Schaltjahre, was sie für Anwendungen, die präzise Datumsberechnungen auf globaler Ebene erfordern, geeigneter machen kann.

Für Excel-Makros und Anwendungen, die eine Integration im Microsoft Office-Ökosystem erfordern, bleibt VBA jedoch eine praktische Wahl. Die Einfachheit, direkt auf Excel-Daten zuzugreifen und diese zu manipulieren, ist ein erheblicher Vorteil. Darüber hinaus bietet `DateAdd()` in VBA für die meisten grundlegenden Datumsberechnungen wie Planung und Erinnerungen eine ausreichende und unkomplizierte Lösung. Seine Syntax ist leicht zu erlernen für Einsteiger, während seine Integration in die breiteren Office-Suiten-Anwendungen seine Relevanz in spezifischen Anwendungsfällen sicherstellt.

Zusammenfassend lässt sich sagen, dass alternative Programmiersprachen möglicherweise modernere Ansätze zur Datumsberechnung bieten, `DateAdd()` in VBA jedoch als Zeugnis für die Durchhaltefähigkeit der Sprache in den Bereichen dient, in denen sie am meisten benötigt wird.
