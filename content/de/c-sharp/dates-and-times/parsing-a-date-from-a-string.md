---
title:                "Einen Datum aus einem String analysieren"
aliases: - /de/c-sharp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:43.095329-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen Datum aus einem String analysieren"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String in C# beinhaltet das Umwandeln von textuellen Darstellungen von Daten und Zeiten in ein `DateTime`-Objekt. Dies ist essentiell für Anwendungen, die Daten und Zeiten in verschiedenen Formaten manipulieren, speichern oder anzeigen müssen, wie beispielsweise Planungs-Apps, Log-Verarbeitungen oder jegliches System, das Dateneingaben von Benutzern oder externen Quellen handhabt.

## Wie geht das:

**Basis-Parsing:**

Die Methoden `DateTime.Parse` und `DateTime.TryParse` sind die ersten Anlaufstellen, um einen String in ein `DateTime` umzuwandeln. Hier ist ein kurzes Beispiel:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Erfolgreich geparst: {parsedDate}");
}
else
{
    Console.WriteLine("Parsen fehlgeschlagen.");
}
// Ausgabe: Erfolgreich geparst: 12.04.2023 00:00:00
```

**Spezifische Kultur angeben:**

Manchmal muss ein Datumsstring in einem spezifischen Kulturformat geparst werden. Dies kann mit der `CultureInfo`-Klasse erreicht werden:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Ausgabe: 12.04.2023 00:00:00
```

**Exaktes Parsen mit einem spezifischen Format:**

Für Szenarien, in denen Daten in einem spezifischen Format vorliegen, das möglicherweise nicht standardmäßig ist, kommt `DateTime.ParseExact` gelegen:

```csharp
string dateString = "Mittwoch, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Ausgabe: 12.04.2023 00:00:00
```

**Mit NodaTime:**

Für noch robustere Datums- und Zeitparsings, ziehen Sie die Verwendung der beliebten Drittanbieterbibliothek NodaTime in Betracht. Sie bietet eine breitere Palette von Datums-/Zeitbehandlungsfähigkeiten:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Parsen fehlgeschlagen.");
}
```

NodaTime bietet umfangreiche Unterstützung für Zeitzonen, Perioden- und Dauerkonzepte sowie viele verschiedene Kalendersysteme, was es zu einer leistungsstarken Wahl für komplexe Datum- und Zeitmanipulationen in .NET-Anwendungen macht.
