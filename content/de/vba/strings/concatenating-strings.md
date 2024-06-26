---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:00.847827-07:00
description: "Wie: VBA bietet eine unkomplizierte Methode zum Verketten von Zeichenketten\
  \ mit dem `&`-Operator oder der `Verketten`-Funktion. Lassen Sie uns beide\u2026"
lastmod: '2024-03-13T22:44:53.708608-06:00'
model: gpt-4-0125-preview
summary: VBA bietet eine unkomplizierte Methode zum Verketten von Zeichenketten mit
  dem `&`-Operator oder der `Verketten`-Funktion.
title: Strings verketten
weight: 3
---

## Wie:
VBA bietet eine unkomplizierte Methode zum Verketten von Zeichenketten mit dem `&`-Operator oder der `Verketten`-Funktion. Lassen Sie uns beide Methoden mit Beispielen erkunden:

1. **Verwenden des `&`-Operators:**

Der `&`-Operator ist die gebräuchlichste Methode zum Verketten von Zeichenketten in VBA. Er ist einfach und effizient für das Verbinden mehrerer Zeichenketten.

```vb.net
Dim Vorname As String
Dim Nachname As String
Vorname = "Jane"
Nachname = "Doe"
' Zeichenketten verketten
Dim vollständigerName As String
vollständigerName = Vorname & " " & Nachname
Debug.Print vollständigerName 'Ausgabe: Jane Doe
```

2. **Verwenden der `Verketten`-Funktion:**

Alternativ erlaubt VBA die Zeichenkettenverkettung mit der `Verketten`-Funktion, die besonders nützlich ist, wenn man mit einem Array von Zeichenketten umgeht oder eine Funktionsyntax bevorzugt.

```vb.net
Dim Begrüßung As String
Dim Name As String
Begrüßung = "Hallo"
Name = "John"
' Zeichenketten mit der Verketten-Funktion verketten
Dim Nachricht As String
Nachricht = Application.WorksheetFunction.Concatenate(Begrüßung, " ", Name, "!")
Debug.Print Nachricht 'Ausgabe: Hallo John!
```

Die Wahl zwischen dem `&`-Operator und der `Verketten`-Funktion hängt von der persönlichen Vorliebe und den spezifischen Anforderungen Ihres Projekts ab.

## Vertiefung
Die Zeichenkettenverkettung ist ein grundlegendes, aber mächtiges Feature in VBA, das seine Wurzeln bis in die Anfänge der Programmiersprachen zurückverfolgt. Die Prävalenz des `&`-Operators in VBA für die Verkettung gegenüber dem `+`-Operator, der in vielen anderen Sprachen verwendet wird, unterstreicht den Fokus von VBA auf explizite Zeichenkettenbehandlung und vermeidet so unbeabsichtigte Datentypeninkongruenzen und Fehler.

Während der `&`-Operator effizient und weit verbreitet ist, glänzt die `Verketten`-Funktion in Szenarien, die mehr Klarheit erfordern oder spezielle Verkettungsfälle behandeln, wie etwa den Umgang mit Arrays. Es ist jedoch wichtig zu beachten, dass moderne Versionen von Excel die `TEXTJOIN`-Funktion eingeführt haben, die zum Verketten von Arrays von Zeichenketten mit einem Trennzeichen effizienter sein kann, obwohl sie nicht direkt Teil von VBA ist.

Bei umfangreichen Zeichenkettenmanipulationen oder leistungskritischen Anwendungen könnten Programmierer Alternativen wie die Verwendung der `StringBuilder`-Klasse in .NET (über COM in VBA zugänglich) erkunden. Dies kann die Leistung erheblich steigern, insbesondere in Schleifen oder beim Verketten einer großen Anzahl von Zeichenketten, aufgrund seiner effizienteren Speichernutzungsmuster.

Letztendlich hängt die Wahl der richtigen Methode zum Verketten von Zeichenketten in VBA von Ihren spezifischen Bedürfnissen, Leistungsüberlegungen und der Lesbarkeit ab. Ob Sie sich für die Einfachheit des `&`-Operators oder die Funktionalität der `Verketten`-Funktion entscheiden, das Verständnis der Implikationen und Effizienz jedes Ansatzes ist entscheidend für die effektive Zeichenkettenmanipulation in VBA.
