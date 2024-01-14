---
title:    "C#: Konvertere en streng til små bokstaver"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor
Det er ofte nyttig å kunne konvertere en streng til små bokstaver når du jobber med tekstbehandling i et program. Dette gjør det enklere å sammenligne tekst eller søke etter bestemte ord, uavhengig av om de er skrevet med store eller små bokstaver.

## Hvordan gjøre det
For å konvertere en streng til små bokstaver i C#, kan du bruke metoden `ToLower()` på en `string`-variabel. Her er et eksempel på hvordan dette kan gjøres:

```C#
string tekst = "DEtte er En TEksT";
string konvertertTekst = tekst.ToLower();

Console.WriteLine(konvertertTekst); // output: dette er en tekst
```

Som du kan se, vil metoden `ToLower()` automatisk konvertere alle bokstavene i strengen til små bokstaver. Dette gjør det enkelt å sammenligne denne strengen med andre strenger eller søke etter spesifikke ord.

## Dypdykk
Når du bruker `ToLower()`-metoden, er det viktig å være klar over at den vil tilpasse seg språkinnstillingene til datamaskinen din. Dette betyr at hvis datamaskinen er satt til å bruke en annen skrift enn latin, vil metoden også konvertere bokstaver fra dette skriftet til små bokstaver.

Det er også verdt å merke seg at `ToLower()` bare vil konvertere bokstavene i selve strengen. Dette betyr at eventuelle tall, spesialtegn eller mellomrom ikke vil bli påvirket. Hvis du ønsker å konvertere hele strengen til små bokstaver, kan du bruke `ToLowerInvariant()`-metoden i stedet.

## Se også
- [Microsoft dokumentasjon for ToLower-metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1)
- [Guide til å jobbe med strenger i C#](https://www.tutlane.com/tutorial/csharp/csharp-string-methods)