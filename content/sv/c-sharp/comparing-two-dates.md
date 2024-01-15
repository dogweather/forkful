---
title:                "Jämföra två datum"
html_title:           "C#: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara användbart i många olika situationer, till exempel när man vill kontrollera om en viss händelse har inträffat före eller efter ett annat datum eller om två datum är likadana.

## Hur man gör
```C#
DateTime date1 = new DateTime(2021, 7, 22);
DateTime date2 = new DateTime(2021, 7, 20);

// Jämför om date1 är lika med date2
if(date1 == date2)
{
  Console.WriteLine("Datumen är likadana.");
}
// Jämför om date1 är tidigare än date2
else if(date1 < date2)
{
  Console.WriteLine("date1 är tidigare än date2.");
}
// Jämför om date1 är senare än date2
else if(date1 > date2)
{
  Console.WriteLine("date1 är senare än date2.");
}
```

Resultat:
```
date1 är senare än date2.
```

## Djupdykning
För att jämföra två datum i C# kan man använda jämförelseoperatorerna "==" (lika med), "<" (mindre än) och ">" (större än). Det är viktigt att notera att ett DateTime-objekt innehåller både datum och tid, vilket innebär att jämförelsen också tar hänsyn till tiden om den är angiven.

För att endast jämföra datumdelen av ett DateTime-objekt kan man använda metoden `date1.Date == date2.Date`.

För att få mer precisa jämförelser finns också möjligheten att använda DateTime-metoderna `date1.Equals(date2)` eller `date1.CompareTo(date2) == 0`.

## Se även
- [DateTime-strukturen](https://docs.microsoft.com/sv-se/dotnet/api/system.datetime?view=net-5.0)
- [Jämföra värden i C#](https://docs.microsoft.com/sv-se/dotnet/csharp/language-reference/operators/operator-comparisons)