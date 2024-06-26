---
date: 2024-01-26 00:50:05.347551-07:00
description: "Wie man das macht: Beginnen wir mit einem Try-Catch-Block. Es ist so,\
  \ als w\xFCrde man unter einem Seilt\xE4nzer ein Sicherheitsnetz spannen. Wenn sie\u2026"
lastmod: '2024-03-13T22:44:53.895545-06:00'
model: gpt-4-1106-preview
summary: Beginnen wir mit einem Try-Catch-Block.
title: Fehlerbehandlung
weight: 16
---

## Wie man das macht:
Beginnen wir mit einem Try-Catch-Block. Es ist so, als würde man unter einem Seiltänzer ein Sicherheitsnetz spannen. Wenn sie ausrutschen, stürzen sie nicht ab – sie werden gefangen.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] zahlen = {1, 2, 3};
            Console.WriteLine(zahlen[5]);  // Hoppla, der Index liegt außerhalb der Grenzen!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Einen Fehler gefangen: " + e.Message);
        }
    }
}
```

Beispielausgabe, wenn etwas schief geht:
```
Einen Fehler gefangen: Der Index lag außerhalb der Grenzen des Arrays.
```

Jetzt fügen wir einen Finally-Block hinzu – das ist das, was passiert, egal was ist, wie Steuern zahlen.

```C#
try {
    // Potenziell problematischer Code hier
} catch (SomeSpecificException e) {
    // Behandle diesen spezifischen Fehler hier
} finally {
    // Dieser Code läuft, egal was oben passiert
    Console.WriteLine("Das läuft immer.");
}
```

## Tiefgang
Die Fehlerbehandlung ist in C# seit seiner Geburt vorhanden. Im Laufe der Zeit hat sie sich entwickelt. Früher verließen sich Programmierer auf Rückgabecodes oder globale Flags, um Probleme zu signalisieren – umständlich und fehleranfällig.

C# verwendet Ausnahmen, einen moderneren Ansatz. Eine Ausnahme wird ausgelöst, wenn das Unerwartete eintritt, genau wie wenn im Fußball ein Flagge auf das Spiel geworfen wird. Strukturierte Ausnahmebehandlung mit Try, Catch und Finally-Blöcken macht das Verwalten dieser Momente klarer und sauberer als die altmodische Fehlerprüfung.

Alternativen? Sicher. Es gibt den `UnhandledExceptionEventHandler` für Ausnahmen, die durchrutschen. Oder in asynchronem Code dreht sich die Fehlerbehandlung ein wenig auf den Kopf mit `Task`-Objekten, die ihre eigenen Ausnahmen mit sich bringen.

Implementierungsdetails – vergleichbar mit dem Kleingedruckten – sind wichtig. Ausnahmen können kostspielig sein und die Leistung beeinträchtigen, wenn sie wahllos geworfen werden. Daher verwenden wir sie für außergewöhnliche Fälle und nicht für alltägliche Logiksteuerung.

## Siehe auch
- [Offizielle Dokumentation zu Ausnahmen in C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Best Practices für die Ausnahmebehandlung in C#](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
