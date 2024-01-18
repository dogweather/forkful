---
title:                "Analysering av en dato fra en streng"
html_title:           "C#: Analysering av en dato fra en streng"
simple_title:         "Analysering av en dato fra en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Hva & hvorfor?
Å parsere en dato fra en streng er ganske enkelt å konvertere en tekststreng som representerer en dato til et egnet dataprogram-format. Dette er nyttig for programmerere fordi det lar dem lese og behandle datoer på en mer strukturert og nøyaktig måte.

# Hvordan:
Det er ganske enkelt å parse en dato fra en streng i C#. Det finnes flere innebygde funksjoner og metoder som gjør dette enkelt og raskt. Her er et eksempel på hvordan du kan gjøre det:

```C#
string dato = "2021-01-15"; // Tekststreng som representerer en dato
DateTime parsedDate = DateTime.Parse(dato); // Parse strengen til en DateTime-variabel
Console.WriteLine(parsedDate); // Skriv ut den parsede datoen
// Output: 1/15/2021 12:00:00 AM
```

Som du kan se, kan C# enkelt håndtere konverteringen for oss. Det eksisterer også flere alternativer, for eksempel `DateTime.TryParse` som vil returnere en boolsk verdi avhengig av om parsingen var vellykket eller ikke.

# Dypdykk:
Parsing av datoer fra strenger er viktig innen programmering for å sikre at data behandles på en strukturert og nøyaktig måte. Det er spesielt nyttig i situasjoner der data kommer fra ulike kilder og kan være i forskjellige formater. Gjennom årene har det dukket opp flere alternative metoder for å gjøre dette, for eksempel regulære uttrykk og til og med egendefinerte parsere. Men takket være de innebygde funksjonene i C#, er det nå enklere enn noensinne å parse datoer fra strenger.

# Se også:
Hvis du ønsker å lære mer om parsing av datoer fra strenger i C#, kan du sjekke ut disse nyttige ressursene:

- [Microsoft Docs: DateTime.Parse Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-5.0)
- [Microsoft Learn: Working with Dates and Times in C#](https://docs.microsoft.com/en-us/learn/paths/csharp-manipulate-date-time/)
- [C# Corner: Parsing Dates In C#](https://www.c-sharpcorner.com/blogs/parsing-dates-in-c-sharp)