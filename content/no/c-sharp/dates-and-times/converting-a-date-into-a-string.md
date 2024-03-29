---
date: 2024-01-20 17:36:12.163406-07:00
description: "Konvertering av dato til streng gj\xF8r datoen lesbar for mennesker.\
  \ Dette er nyttig for visning, logging eller formatert utveksling mellom systemer."
lastmod: '2024-03-13T22:44:40.808271-06:00'
model: gpt-4-1106-preview
summary: "Konvertering av dato til streng gj\xF8r datoen lesbar for mennesker. Dette\
  \ er nyttig for visning, logging eller formatert utveksling mellom systemer."
title: Konvertere en dato til en streng
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Konvertering av dato til streng gjør datoen lesbar for mennesker. Dette er nyttig for visning, logging eller formatert utveksling mellom systemer.

## How to: (Slik gjør du:)
```C#
using System;
using System.Globalization;

public class DateToStringExample
{
    public static void Main()
    {
        DateTime now = DateTime.Now;
        string formattedDate = now.ToString("yyyy-MM-dd");
        Console.WriteLine(formattedDate); // Output: 2023-04-12 (Today's date in the format)
        
        // With culture
        CultureInfo norwegianCulture = new CultureInfo("nb-NO");
        string formattedDateWithCulture = now.ToString("d", norwegianCulture);
        Console.WriteLine(formattedDateWithCulture); // Output: 12.04.2023 (Norwegian format)
    }
}
```

## Deep Dive (Dypdykk)
Konvertering av datoer til strenger er ikke et nytt konsept. Det går tilbake til begynnelsen av programmering. Hvorfor? Formatering av datoer til menneskelig lesbar form er grunnleggende.

Alternativer inkluderer å bruke standardformater eller tilpasse dine egne. `DateTime.ToString()` er kraftig i C#. Det håndterer ulike kulturer ved hjelp av `CultureInfo`. For eksempel gjør `nb-NO` (Norsk Bokmål) det mulig for datoen å vises som folk i Norge ville forvente.

En implementeringsdetalj: `ToString()` bruker `DateTimeFormatInfo` fra den angitte `CultureInfo` for å formatere datoen. Uten `CultureInfo`, bruker den systemets kulturinnstillinger.

## See Also (Se Også)
- [Standard Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
