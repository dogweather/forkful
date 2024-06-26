---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:25.389715-07:00
description: "Wie geht das: In VBA bietet das `Dictionary`-Objekt eine Funktionalit\xE4\
  t, die assoziativen Arrays \xE4hnelt. Um es nutzen zu k\xF6nnen, m\xFCssen Sie zun\xE4\
  chst eine\u2026"
lastmod: '2024-03-13T22:44:53.709679-06:00'
model: gpt-4-0125-preview
summary: "In VBA bietet das `Dictionary`-Objekt eine Funktionalit\xE4t, die assoziativen\
  \ Arrays \xE4hnelt."
title: Verwendung von assoziativen Arrays
weight: 15
---

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
