---
date: 2024-01-20 17:36:14.305520-07:00
description: "How to (Wie man's macht): C# bietet die `ToString`-Methode f\xFCr das\
  \ `DateTime`-Objekt, um Datumsangaben in Strings umzuwandeln. Sehen wir uns das\
  \ an."
lastmod: '2024-03-13T22:44:53.899819-06:00'
model: gpt-4-1106-preview
summary: "C# bietet die `ToString`-Methode f\xFCr das `DateTime`-Objekt, um Datumsangaben\
  \ in Strings umzuwandeln."
title: Datum in einen String umwandeln
weight: 28
---

## How to (Wie man's macht):
C# bietet die `ToString`-Methode für das `DateTime`-Objekt, um Datumsangaben in Strings umzuwandeln. Sehen wir uns das an:

```C#
using System;

public class DateFormatter
{
    public static void Main(string[] args)
    {
        DateTime now = DateTime.Now;
        string dateFormat1 = now.ToString("dd.MM.yyyy");
        string dateFormat2 = now.ToString("dddd, dd MMMM yyyy");
        
        Console.WriteLine(dateFormat1); // z.B. "05.04.2023"
        Console.WriteLine(dateFormat2); // z.B. "Mittwoch, 05 April 2023"
    }
}
```

## Deep Dive (Tiefere Einblicke):
Datum in String zu konvertieren, ist nichts Neues. Schon früh erkannten Programmierersprachen die Notwendigkeit, Daten menschenlesbar zu machen. C# hat das vereinfacht durch `DateTime.ToString()`. Es gibt Alternativen wie `String.Format()` oder Interpolation, die `ToString()` implizit aufrufen.

Details der Implementierung:

- Kulturabhängig: `ToString()` kann kulturspezifische Formatierungen nutzen, wie `en-US` oder `de-DE`.
- Geschwindigkeit: Direkte Methoden wie `ToString("yyyyMMdd")` sind meist schneller als `String.Format()`.
- Anpassbar: Mit eigenen Format-Strings lässt sich fast jedes gewünschte Format erzeugen.

## See Also (Siehe auch):
- MSDN Dokumentation zur `DateTime`-Klasse: [docs.microsoft.com/de-de/dotnet/api/system.datetime](https://docs.microsoft.com/de-de/dotnet/api/system.datetime)
- Standard- und benutzerdefinierte Datums- und Zeitformatzeichenfolgen: [docs.microsoft.com/de-de/dotnet/standard/base-types/standard-date-and-time-format-strings](https://docs.microsoft.com/de-de/dotnet/standard/base-types/standard-date-and-time-format-strings)
- Darstellung von Daten und Uhrzeiten in unterschiedlichen Kulturen: [docs.microsoft.com/de-de/dotnet/standard/globalization-localization](https://docs.microsoft.com/de-de/dotnet/standard/globalization-localization)
