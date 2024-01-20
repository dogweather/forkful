---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Umgang mit Datumskonvertierung in C# 

## Was & Warum?
Die Konvertierung eines Datums in eine Zeichenkette (String) bedeutet einfach, das Datumsformat in eine lesbare Textform zu ändern. Programmierer tun dies häufig, um die Datumsausgabe nach Bedarf zu formatieren.

## Wie macht man das:
Wir verwenden die `ToString()` Methode, um ein Datum in einen String zu konvertieren. Hier ist ein einfacher Code zum Konvertieren eines Datums in einen String:

```C# 
using System;

public class Program
{
    public static void Main()
    {
      DateTime currentDate = DateTime.Now; // Aktuelles Datum und Zeit
      Console.WriteLine(currentDate.ToString("dd/MM/yyyy")); // Formatierung des Datums
    }
}
```

Die Ausgabe könnte so aussehen:

``` 
27/10/2021
```

## Deep Dive
Historisch gesehen haben Programmierer immer nach Möglichkeiten gesucht, Daten auf eine für Menschen lesbare Weise darzustellen. Daher wurde das Konzept der Datumsformatierung eingeführt.

Abgesehen von `ToString()`, bietet `.Net` die `Date` und `Time` Struktur an, die ebenfalls zur Formatierung eingesetzt werden kann. Hier ist eine alternative Möglichkeit:

```C# 
DateTime currentDate = DateTime.Now; 
string dateString = $"{currentDate:dd MMMM yyyy}";
```

In Bezug auf die Implementierung ist es wichtig zu wissen, dass die `ToString()` Methode unterschiedliche Formate akzeptiert. Z.B. "MM/dd/yyyy" erstellt einen String in einem Format, das in den USA weit verbreitet ist, während "dd/MM/yyyy" in Ländern wie Deutschland verbreitet ist.

## Siehe auch
- [Microsoft: Custom DateTimeFormat strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)