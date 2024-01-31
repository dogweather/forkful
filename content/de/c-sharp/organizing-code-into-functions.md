---
title:                "Code in Funktionen organisieren"
date:                  2024-01-26T01:09:06.787921-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"

category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren ist, wie LEGO-Steine in Behälter zu sortieren – es macht das Finden und Verwenden einfacher. Wir tun dies, um Wiederholungen zu vermeiden, das Verständnis zu vereinfachen und die Wartung weniger kopfschmerzhaft zu machen.

## Wie man es macht:
Stellen Sie sich vor, Sie haben Code, der mehrmals einen Gruß ausdruckt. Ohne Funktionen ist es ein Durcheinander. Mit Funktionen ist es ordentlich.

```C#
// Ohne Funktionen - repetitiv
Console.WriteLine("Hallo, Amy!");
Console.WriteLine("Hallo, Bob!");
Console.WriteLine("Hallo, Charlie!");

// Mit Funktionen - sauberer
void Greet(string name) {
    Console.WriteLine($"Hallo, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

Die Ausgabe ist die gleiche, aber die zweite Version ist viel aufgeräumter.

## Tiefer eintauchen
Früher, in den Tagen der Assemblersprache, würden Sie mit GOTO zu verschiedenen Codebereichen springen – chaotisch und schwer zu verfolgen. Funktionen sind ein großer Fortschritt, wie organisierte Schubladen in einem Werkzeugkasten. Alternativen? Sicher. Es gibt Methoden, die Funktionen im Kontext einer Klasse sind. Dann gibt es Lambdas und Inline-Funktionen für schnelle, einmalige Aufgaben.

Über die Implementierung – kleine, fokussierte Funktionen sind Gold wert. Sie sind leichter zu testen und zu debuggen. Große Funktionen mit vielen Verantwortlichkeiten können monströs werden und verdienen den zweifelhaften Titel "Spaghetticode". Bleiben Sie bei einer Aufgabe pro Funktion; Sie werden sich später danken.

## Siehe auch
Für mehr über Funktionen und beste Praktiken, sehen Sie sich an:

- Clean Code von Robert C. Martin: Prinzipien, um Ihre Funktionen aufgeräumt zu halten.
- Refactoring von Martin Fowler: Wege, um bestehenden Code zu verbessern.
- Microsoft C# Leitfaden zu Methoden: https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/classes-and-structs/methods
