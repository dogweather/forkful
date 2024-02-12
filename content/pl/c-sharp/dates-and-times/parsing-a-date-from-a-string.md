---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases:
- /pl/c-sharp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:49.071516-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Przetwarzanie daty z ciągu znaków w C# polega na konwertowaniu tekstowych reprezentacji dat i czasów na obiekt `DateTime`. Jest to kluczowe dla aplikacji, które potrzebują manipulować, przechowywać lub wyświetlać daty i czasy w różnych formatach, takich jak aplikacje do planowania, procesory logów lub dowolny system obsługujący wprowadzanie dat przez użytkowników lub z zewnętrznych źródeł.

## Jak to zrobić:

**Podstawowe przetwarzanie:**

Metody `DateTime.Parse` i `DateTime.TryParse` to podstawowe opcje do konwertowania ciągu znaków na `DateTime`. Oto szybki przykład:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Pomyślnie przetworzono: {parsedDate}");
}
else
{
    Console.WriteLine("Nie udało się przetworzyć.");
}
// Wyjście: Pomyślnie przetworzono: 4/12/2023 12:00:00 AM
```

**Określanie kultury:**

Czasami konieczne jest przetworzenie ciągu daty, który jest w określonym formacie kulturowym. Można to osiągnąć za pomocą klasy `CultureInfo`:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Wyjście: 4/12/2023 12:00:00 AM
```

**Dokładne przetwarzanie z określonym formatem:**

W scenariuszach, gdy daty są podane w określonym formacie, który może nie być standardowy, przydaje się `DateTime.ParseExact`:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Wyjście: 4/12/2023 12:00:00 AM
```

**Korzystanie z NodaTime:**

Do jeszcze bardziej zaawansowanego przetwarzania dat i czasów rozważ użycie popularnej biblioteki stron trzecich NodaTime. Zapewnia ona szerszy zakres możliwości obsługi dat/czasu:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if(parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Nie udało się przetworzyć.");
}
```

NodaTime oferuje obszerną pomoc w zakresie stref czasowych, koncepcji okresów i trwania, oraz wielu różnych systemów kalendarzowych, co czyni go potężnym wyborem dla złożonej manipulacji datą i czasem w aplikacjach .NET.
