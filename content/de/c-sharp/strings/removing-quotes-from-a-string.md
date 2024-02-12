---
title:                "Anführungszeichen aus einem String entfernen"
aliases: - /de/c-sharp/removing-quotes-from-a-string.md
date:                  2024-01-26T03:38:11.066663-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Entfernen von Anführungszeichen aus einem String in C# bedeutet, dass Sie diese lästigen doppelten (`"`) oder einfachen (`'`) Anführungszeichen, die Ihren Text umschließen, herausnehmen. Programmierer tun dies, um Daten zu säubern, sie für die Eingabe in Datenbanken vorzubereiten oder Strings für die weitere Verarbeitung sicher zu machen, damit nichts schiefgeht, wenn ein verirrtes Anführungszeichen auftaucht.

## Wie zu:
```csharp
string withQuotes = "\"Hallo, Welt!\"";
Console.WriteLine($"Original: {withQuotes}");

// Doppelte Anführungszeichen entfernen
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"Ohne Doppelte Anführungszeichen: {withoutDoubleQuotes}");

// Einfache Anführungszeichen entfernen (vorausgesetzt, Ihr String hatte sie überhaupt zu Beginn)
string withSingleQuotes = "'Hallo, Welt!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"Ohne Einfache Anführungszeichen: {withoutSingleQuotes}");
```

Ausgabe:
```
Original: "Hallo, Welt!"
Ohne Doppelte Anführungszeichen: Hallo, Welt!
Ohne Einfache Anführungszeichen: Hallo, Welt!
```

## Vertiefung
Das Konzept, Anführungszeichen zu entfernen, ist nicht neu oder besonders komplex, aber es ist entscheidend, weil Anführungszeichen oft verwendet werden, um Strings zu begrenzen. Wenn ein String mit unescaped Anführungszeichen in einem Codeblock oder einer Datendatei enthalten ist, könnte er den String vorzeitig beenden, was zu Fehlern oder Sicherheitsproblemen wie Einschleusungsangriffen führen kann.

Historisch gesehen war der Umgang mit Anführungszeichen Teil des Validierungs- und Sanierungsprozesses bei der Datenverarbeitung. Obwohl die Methode `.Replace()` unkompliziert ist, um Anführungszeichen aus einem einfachen String zu entfernen, benötigen Sie möglicherweise fortgeschrittenere Techniken wie reguläre Ausdrücke, um komplexere Szenarien zu behandeln, wie z.B. verschachtelte Anführungszeichen oder bedingte Entfernungen.

Alternativen zu `.Replace()` umfassen Methoden aus der `Regex`-Klasse, wenn Sie eine feinkörnige Kontrolle benötigen oder mit Mustern statt mit festen Zeichen arbeiten. Zum Beispiel könnte `Regex.Unescape()` nützlich sein, wenn Sie mit escaped Zeichen umgehen.

Implementierungsweise sollten Sie sich daran erinnern, dass Strings in C# unveränderlich sind, was bedeutet, dass jedes Mal, wenn Sie `.Replace()` verwenden, ein neuer String erstellt wird. Das ist kein Problem bei kleinen oder einmaligen Operationen, aber etwas, das leistungstechnisch für große oder zahlreiche Strings berücksichtigt werden sollte.

## Siehe auch:
- [String.Replace Methoden Dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [Reguläre Ausdrücke in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Best Practices für sichere String-Handhabung](https://www.owasp.org/index.php/Data_Validation)
