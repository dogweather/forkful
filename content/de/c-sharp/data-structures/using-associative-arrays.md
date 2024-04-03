---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:07.903619-07:00
description: "Assoziative Arrays, oder W\xF6rterb\xFCcher in C#, erm\xF6glichen es\
  \ Ihnen, Schl\xFCssel-Wert-Paare zu speichern und zu verwalten. Sie sind Ihre erste\
  \ Wahl, wenn Sie\u2026"
lastmod: '2024-03-13T22:44:53.881232-06:00'
model: gpt-4-0125-preview
summary: "Assoziative Arrays, oder W\xF6rterb\xFCcher in C#, erm\xF6glichen es Ihnen,\
  \ Schl\xFCssel-Wert-Paare zu speichern und zu verwalten."
title: Verwendung von assoziativen Arrays
weight: 15
---

## Wie:
In C# arbeiten Sie mit assoziativen Arrays mittels der `Dictionary<TKey, TValue>` Klasse. Hier ist ein schnelles Beispiel, um Ihnen den Einstieg zu erleichtern:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Ein Wörterbuch erstellen
        Dictionary<string, int> fruchtkorb = new Dictionary<string, int>();

        // Schlüssel-Wert-Paare hinzufügen
        fruchtkorb.Add("Äpfel", 5);
        fruchtkorb.Add("Orangen", 10);

        // Einen Wert mit seinem Schlüssel abrufen
        Console.WriteLine("Äpfel: " + fruchtkorb["Äpfel"]);
        
        // Einen Wert aktualisieren
        fruchtkorb["Äpfel"] = 7;
        Console.WriteLine("Aktualisierte Äpfel: " + fruchtkorb["Äpfel"]);
        
        // Ein Schlüssel-Wert-Paar entfernen
        fruchtkorb.Remove("Orangen");

        // Über das Wörterbuch iterieren
        foreach (var paar in fruchtkorb)
        {
            Console.WriteLine(paar.Key + ": " + paar.Value);
        }
    }
}
```
Beispielausgabe:
```
Äpfel: 5
Aktualisierte Äpfel: 7
Äpfel: 7
```

Dieses Beispiel zeigt, wie man ein Wörterbuch erstellt, Elemente hinzufügt, abruft, aktualisiert und entfernt sowie darüber iteriert.

## Vertiefung
Das Konzept der assoziativen Arrays geht auf ihren Einsatz in Skriptsprachen wie Perl und PHP zurück, wo sie Flexibilität bei der Verwaltung von Datensammlungen bieten. In C# ist `Dictionary<TKey, TValue>` die de facto Implementierung, eingeführt in .NET Framework 2.0. Es speichert Daten in einer Hashtabelle, was effiziente Suchvorgänge, Hinzufügungen und Löschungen gewährleistet.

Es ist jedoch zu beachten, dass Wörterbücher, obwohl sie unglaublich vielseitig sind, nicht immer Ihre beste Wahl sein könnten. Für die Beibehaltung geordneter Sammlungen könnten Sie `SortedDictionary<TKey, TValue>` oder `SortedList<TKey, TValue>` in Betracht ziehen, welche eine sortierte Ordnung auf Kosten langsamerer Einfüge- und Entfernungsoperationen bieten. Für Szenarien, die Thread-Sicherheit erfordern, fügt `ConcurrentDictionary<TKey, TValue>` einen Overhead hinzu, stellt jedoch einen sicheren Zugriff von mehreren Threads ohne manuelle Sperrung sicher.

Letztendlich hängt die Wahl einer Implementierung assoziativer Arrays in C# von Ihren spezifischen Bedürfnissen bezüglich Ordnung, Leistung und Thread-Sicherheit ab.
