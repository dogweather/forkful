---
title:                "Reguläre Ausdrücke verwenden"
aliases:
- /de/vba/using-regular-expressions/
date:                  2024-02-01T22:04:57.408193-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguläre Ausdrücke verwenden"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/using-regular-expressions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (regex) in Visual Basic for Applications (VBA) bieten eine leistungsstarke Möglichkeit, um Zeichenketten zu durchsuchen, abzugleichen und zu manipulieren. Programmierer nutzen sie für Aufgaben wie Datenvalidierung, Parsing und Transformation wegen ihrer Flexibilität und Effizienz beim Umgang mit komplexen Zeichenkettenmustern.

## Wie geht das:

Um reguläre Ausdrücke in VBA zu verwenden, müssen Sie zuerst die Microsoft VBScript Regular Expressions-Bibliothek aktivieren. Gehen Sie im VBA-Editor zu `Extras` -> `Verweise`, und aktivieren Sie `Microsoft VBScript Regular Expressions 5.5`.

Hier ist ein einfaches Beispiel, um zu finden, ob ein Muster in einer Zeichenkette vorhanden ist:

```vb
Sub MusterFinden()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' Sucht nach dem Wort "is"
    End With
    
    Dim testString As String
    testString = "This is a test string."
    
    If regex.Test(testString) Then
        MsgBox "Muster gefunden."
    Else
        MsgBox "Muster nicht gefunden."
    End If
End Sub
```

Um ein Muster in einer Zeichenkette zu ersetzen:

```vb
Sub MusterErsetzen()
    Dim regex As Object, ersetzterString As String
    Set regex = CreateObject("VBScript.RegExp")
    
    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' Passt auf jedes Leerzeichen
    End With
    
    ersetzterString = regex.Replace("This is a test string.", "_")
    MsgBox ersetzterString  ' Ausgabe: "This_is_a_test_string."
End Sub
```

## Tiefergehende Betrachtung

Die Einbeziehung von regulären Ausdrücken in Programmiersprachen führt oft zurück auf Unix-Werkzeuge aus den 1970er Jahren. VBA integrierte Regex durch die VBScript Regular Expressions-Bibliothek, was ihre Bedeutung bei Textverarbeitungsaufgaben unterstreicht, selbst in Anwendungen, die nicht typischerweise mit intensiver Textmanipulation in Verbindung gebracht werden wie Excel oder Access.

Trotz ihrer Stärke können Regex in VBA manchmal weniger intuitiv oder leistungsfähig sein im Vergleich zu moderneren Implementierungen in Sprachen wie Python oder JavaScript. Zum Beispiel bietet Pythons `re` Modul umfassende Unterstützung für benannte Gruppen und ausgefeiltere Musterabgleichsfunktionen, was einen saubereren und potenziell besser lesbaren Ansatz bietet. Wenn man jedoch im VBA-Ökosystem arbeitet, bleiben reguläre Ausdrücke ein unschätzbares Werkzeug für Aufgaben, die Musterabgleich oder Textmanipulation erfordern. Der Effizienzverlust ist oft vernachlässigbar angesichts der Bequemlichkeit und Fähigkeiten, die Regex auf den Tisch bringt, wenn es um den Umgang mit Zeichenketten in Office-Anwendungen geht.
