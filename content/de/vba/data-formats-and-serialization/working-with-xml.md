---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:41.587832-07:00
description: "Die Arbeit mit XML in Visual Basic f\xFCr Anwendungen (VBA) beinhaltet\
  \ das Parsen, Erstellen und Modifizieren von XML-Dokumenten im Kontext von Microsoft\u2026"
lastmod: '2024-03-13T22:44:53.745579-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit XML in Visual Basic f\xFCr Anwendungen (VBA) beinhaltet das\
  \ Parsen, Erstellen und Modifizieren von XML-Dokumenten im Kontext von Microsoft\
  \ Office-Anwendungen."
title: Arbeiten mit XML
weight: 40
---

## Was & Warum?

Die Arbeit mit XML in Visual Basic für Anwendungen (VBA) beinhaltet das Parsen, Erstellen und Modifizieren von XML-Dokumenten im Kontext von Microsoft Office-Anwendungen. Programmierer greifen auf diese Fähigkeit zurück, um Office-Anwendungen mit Webdiensten oder anderen Datenquellen, die XML aussenden, zu integrieren und so den Datenaustausch und die Berichtsfunktionen zu erleichtern.

## Wie:

Um mit dem Interagieren mit XML zu beginnen, verwendet man üblicherweise das `MSXML2.DOMDocument`-Objekt. Diese Schnittstelle ermöglicht es Ihnen, XML-Dokumente zu laden, zu parsen und zu navigieren. Unten finden Sie ein einfaches Beispiel, das zeigt, wie man eine XML-Datei lädt, ihre Struktur durchläuft und Attribute sowie Textinhalte liest.

```basic
' Zunächst stellen Sie sicher, dass Sie die Referenz auf "Microsoft XML, v6.0" über Werkzeuge -> Referenzen hinzugefügt haben
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Pfad\Zu\Ihrer\Datei.xml") ' Laden Sie Ihre XML-Datei

' Überprüfen Sie, ob das XML erfolgreich geladen wurde
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Fehler beim Laden von XML:" & xmlDoc.parseError.reason
Else
    ' Navigieren und Elemente lesen
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath, um den ersten <title> innerhalb von <book> zu finden
    MsgBox book.Text ' Zeigt den Titeltext an
End If
```

Im obigen Beispielcode erstellen wir eine Instanz von `MSXML2.DOMDocument60`, laden eine XML-Datei und überprüfen dann auf Fehler. Wenn keine Fehler gefunden werden, navigieren wir zu einem bestimmten Knoten mithilfe von XPath und zeigen dessen Textinhalt an.

## Tiefer Eintauchen:

Die Integration von XML-Fähigkeiten in VBA reicht zurück bis in die frühen 2000er Jahre, als der Bedarf wuchs, dass Office-Anwendungen mit Webdaten und -diensten interagieren. Die `MSXML`-Bibliothek oder Microsoft XML Core Services hat sich über die Jahre hinweg weiterentwickelt, wobei `MSXML2.DOMDocument60` eine der neuesten Versionen ist, deren Verwendung aufgrund verbesserter Leistungs- und Sicherheitsmerkmale empfohlen wird.

Obwohl leistungsfähig, gelten die XML-Verarbeitungsfähigkeiten von VBA im Vergleich zu modernen Programmierumgebungen wie Pythons XML.etree oder C#s LINQ to XML als weniger effizient und umständlicher. Die inhärente Wortreichheit von VBA und die Notwendigkeit, Referenzen manuell hinzuzufügen und zu verwalten, können eine schnelle Entwicklung behindern. Darüber hinaus führt die Einführung von JSON als einem leichtgewichtigeren Daten-Austauschformat dazu, dass viele Programmierer und Anwendungen sich von XML abwenden, es sei denn, die Interoperabilität mit Altsystemen oder bestimmten Unternehmensdiensten macht dessen Verwendung notwendig.

Jedoch bleibt für Aufgaben, die das Parsen oder Generieren von XML-Dokumenten im Kontext der Microsoft Office-Automatisierung erfordern, die Nutzung der XML-Verarbeitungsfunktionen von VBA ein machbarer und manchmal notwendiger Ansatz. Dies stellt einen Ausgleich dar zwischen dem Zugang zum umfangreichen Funktionssatz von Office-Anwendungen und den durch XML bereitgestellten Fähigkeiten zur strukturierten Datenmanipulation.
