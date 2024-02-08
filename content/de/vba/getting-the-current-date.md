---
title:                "Das aktuelle Datum abrufen"
aliases:
- de/vba/getting-the-current-date.md
date:                  2024-02-01T21:54:35.055810-07:00
model:                 gpt-4-0125-preview
simple_title:         "Das aktuelle Datum abrufen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/getting-the-current-date.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

In Visual Basic for Applications (VBA) ist das Abrufen des aktuellen Datums eine gängige Aufgabe, die es Programmierern ermöglicht, dynamisch mit Daten in ihren Makros oder Anwendungen zu arbeiten. Diese Funktionalität ist entscheidend für Operationen wie Logging, Zeitstempel für Transaktionen oder das Durchführen datumsbasierter Berechnungen.

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
