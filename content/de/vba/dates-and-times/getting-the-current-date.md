---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:35.055810-07:00
description: "Wie: Das Abrufen des aktuellen Datums in VBA ist unkompliziert und erfolgt\
  \ mittels der `Date`-Funktion, w\xE4hrend die `Now`-Funktion sowohl das aktuelle\u2026"
lastmod: '2024-03-13T22:44:53.729893-06:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in VBA ist unkompliziert und erfolgt mittels\
  \ der `Date`-Funktion, w\xE4hrend die `Now`-Funktion sowohl das aktuelle Datum als\
  \ auch die Uhrzeit liefert."
title: Das aktuelle Datum abrufen
weight: 29
---

## Wie:
Das Abrufen des aktuellen Datums in VBA ist unkompliziert und erfolgt mittels der `Date`-Funktion, während die `Now`-Funktion sowohl das aktuelle Datum als auch die Uhrzeit liefert. So können Sie mit beidem arbeiten:

```vb
Sub GetCurrentDate()
    ' Verwendung der Date-Funktion, um das aktuelle Datum zu erhalten
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "Aktuelles Datum: "; currentDate
    
    ' Verwendung der Now-Funktion, um das aktuelle Datum und die Uhrzeit zu erhalten
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "Aktuelles Datum und Uhrzeit: "; currentDateTime
End Sub
```

Wenn Sie dieses Makro ausführen, gibt die `Debug.Print`-Methode das aktuelle Datum und das aktuelle Datum sowie die Uhrzeit im Direktfenster im VBA-Editor aus. Zum Beispiel:

```
Aktuelles Datum: 12.4.2023
Aktuelles Datum und Uhrzeit: 12.4.2023 15:45:22
```

Beachten Sie, dass das Datumsformat je nach den Systemeinstellungen des Computers des Benutzers variieren kann.

## Vertiefung
Die `Date`- und `Now`-Funktionen kapseln die Komplexität des Umgangs mit Datum und Uhrzeit in Visual Basic for Applications ein, indem sie eine Anwendungsebene der Abstraktion bieten, die das Arbeiten mit Daten einfach und intuitiv macht. Historisch gesehen war der Umgang mit Datum und Uhrzeit in der Programmierung mit Herausforderungen verbunden, einschließlich der Handhabung verschiedener Zeitzonen, der Sommerzeitumstellungen und verschiedener Datumsformate.

In VBA verlassen sich diese Funktionen auf das zugrunde liegende Systemdatum und die -uhrzeit, was bedeutet, dass sie von der Lokalität und den Systemeinstellungen des Benutzers beeinflusst werden. Es ist ein zweischneidiges Schwert, das eine Konsistenz mit der Umgebung des Benutzers gewährleistet, aber auch eine sorgfältige Handhabung der Lokalisierung und Zeitzoneinstellungen in globalen Anwendungen erfordert.

Obwohl die Datum- und Uhrzeitfunktionen von VBA für viele Anwendungen, insbesondere im Bereich der Office-Automatisierung, vollkommen geeignet sind, können sie für komplexere Anwendungen wie Hochfrequenzhandelssysteme oder wissenschaftliche Simulationen an Präzision oder Granularität mangeln. In solchen Fällen könnten andere Programmierumgebungen oder Sprachen wie Python oder C# fortgeschrittenere Bibliotheken zur Datum- und Zeitmanipulation bieten.

Dennoch bieten die `Date`- und `Now`-Funktionen von VBA für die überwiegende Mehrheit der Aufgaben im Zusammenhang mit Daten und Uhrzeiten im Kontext von Excel, Word oder anderen Office-Anwendungen eine Balance aus Einfachheit, Leistung und Benutzerfreundlichkeit, die schwer zu übertreffen ist.
