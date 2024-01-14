---
title:    "C#: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Datumswerten kann in vielen Programmierprojekten nützlich sein, zum Beispiel bei der Planung von Terminen oder der Verarbeitung von Daten. Durch das Verständnis der Berechnungsmethoden können wir präzisere Ergebnisse erzielen und unsere Programmierfähigkeiten erweitern.

## Wie man das macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die DateTime Klasse in C# verwenden. Hier ist ein Beispiel, um 7 Tage in die Zukunft zu berechnen und das Ergebnis in der Konsole auszugeben:

```C#
DateTime zukunft = DateTime.Now.AddDays(7);
Console.WriteLine(zukunft);
```

Das Ergebnis wird in folgendem Format ausgegeben: "Monat/Tag/Jahr 12:00:00 AM". Für die Vergangenheit können wir stattdessen die Subtract-Methode verwenden. Hier ist ein Beispiel, um 3 Monate in die Vergangenheit zu berechnen:

```C#
DateTime vergangenheit = DateTime.Now.Subtract(new TimeSpan(90, 0, 0, 0));
Console.WriteLine(vergangenheit);
```

Dieses Ergebnis wird in derselben Formatierung wie oben angezeigt. Wir können auch weitere Methoden verwenden, um spezifischere Berechnungen durchzuführen, wie z.B. das AddYears oder AddMonths. Es ist wichtig zu beachten, dass die DateTime Klasse auch die Berücksichtigung von Schaltjahren und unterschiedlichen Monatslängen ermöglicht.

## Tiefes Eintauchen

Wenn wir uns tiefer mit dem Berechnen von Datumswerten auseinandersetzen möchten, können wir uns mit dem DateTime-Strukturdesign und den zugrunde liegenden Berechnungsmethoden beschäftigen. Es gibt auch verschiedene Optionen und Einstellungen, die wir bei der Verwendung der DateTime-Klasse beachten sollten, um die Genauigkeit und Effizienz unserer Berechnungen zu verbessern.

## Siehe auch 

- [Microsoft Dokumentation: DateTime Klasse in C#](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-5.0)
- [Tutorial: Einstieg in die DateTime-Klasse in C#](https://www.c-sharpcorner.com/article/get-to-know-the-datetime-class-in-c-sharp/)
- [C# Programmierforum: Fragen und Antworten zu DateTime](https://stackoverflow.com/questions/tagged/datetime?fbclid=IwAR1CajGFXvSfQMOAr9wPvVqOZfPgNk7CPH7odzU45SUeOaDMkVNFo_dDu9E)