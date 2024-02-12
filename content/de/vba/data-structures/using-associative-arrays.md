---
title:                "Verwendung von assoziativen Arrays"
date:                  2024-02-01T22:04:25.389715-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/vba/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, oft auch als Wörterbücher in Visual Basic für Anwendungen (VBA) bekannt, ermöglichen es Programmierern, Sammlungen von Schlüssel-Wert-Paaren zu erstellen. Diese Funktion ist entscheidend für effiziente Datenspeicherung und -abruf und bietet eine flexiblere und intuitivere Möglichkeit, Daten zu verwalten, als dies mit traditionellen Array-Indizes der Fall ist.

## Wie geht das:

In VBA bietet das `Dictionary`-Objekt eine Funktionalität, die assoziativen Arrays ähnelt. Um es nutzen zu können, müssen Sie zunächst eine Referenz auf die Microsoft Scripting Runtime hinzufügen:

1. Gehen Sie im VBA-Editor zu Extras > Verweise...
2. Aktivieren Sie "Microsoft Scripting Runtime" und klicken Sie auf OK.

So deklarieren, füllen und greifen Sie auf Elemente in einem `Dictionary` zu:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' Elemente hinzufügen
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Ingenieur"

' Auf Elemente zugreifen
Debug.Print sampleDictionary.Item("Name")  ' Ausgabe: John Doe
Debug.Print sampleDictionary.Item("Age")   ' Ausgabe: 29

' Überprüfen, ob ein Schlüssel existiert
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Schlüssel Occupation existiert"
End If

' Elemente entfernen
sampleDictionary.Remove("Occupation")

' Durch das Wörterbuch iterieren
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## Vertiefung

Das `Dictionary`-Objekt interagiert im Hintergrund mit Komponenten des Windows Scripting Hosts. Daher ist es ein spätgebundenes COM-Objekt, eine in der Vergangenheit gängige Methode, um die Funktionalität von VBA zu erweitern. Seine Verwendung in VBA kann die Fähigkeit der Sprache, komplexe Datensätze zu manipulieren, erheblich verbessern, ohne eine starre Struktur zu erzwingen, wie es bei traditionellen Arrays oder Excel-Bereichen der Fall ist.

Eine Einschränkung, die zu beachten ist, ist der Zugriff auf das `Dictionary`, der das Setzen einer Referenz auf die Microsoft Scripting Runtime erfordert, was die Verteilung Ihrer VBA-Projekte erschweren kann. Alternativen wie Collections sind innerhalb von VBA vorhanden, aber sie verfügen nicht über einige der Schlüsseleigenschaften des `Dictionary`s, wie z.B. die Möglichkeit, leicht zu überprüfen, ob ein Schlüssel existiert, ohne einen Fehler auszulösen.

In neueren Programmierkontexten bieten Sprachen wie Python eingebaute Unterstützung für assoziative Arrays (auch dort als Wörterbücher bekannt) ohne die Notwendigkeit, externe Referenzen hinzuzufügen. Diese eingebaute Unterstützung vereinfacht den Prozess und bietet von Anfang an fortgeschrittenere Funktionen. Jedoch bleibt innerhalb der Grenzen von VBA und für spezifische Anwendungen, die auf die Automatisierung von Aufgaben in der Microsoft Office Suite abzielen, die Verwendung des `Dictionary`-Objekts eine leistungsstarke und relevante Methode für datenstrukturähnliche assoziative Arrays.
