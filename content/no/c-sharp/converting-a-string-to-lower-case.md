---
title:                "C#: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig når du ønsker å sammenligne strenger uten å ta hensyn til store og små bokstaver. Dette kan også gjøre det enklere å søke gjennom tekst ved å ignorere forskjeller i bokstavstørrelse.

## Hvordan utføre konverteringen

For å konvertere en streng til små bokstaver i C#, kan du bruke metoden `ToLower()` som er tilgjengelig på alle strenger. Her er et eksempel på hvordan du kan bruke denne metoden:

```C#
string tekst = "HeLlo wOrld";
string konvertertTekst = tekst.ToLower();
Console.WriteLine(konvertertTekst);
```

Dette vil gi følgende utskrift:

```
hello world
```

Som du kan se, har alle bokstavene i strengen blitt konvertert til små bokstaver. Det er viktig å merke seg at dette også gjelder for spesialtegn og bokstaver med aksenter.

## Dykk dypere

Nå som vi har sett på en enkel måte å konvertere en streng til små bokstaver, la oss dykke litt dypere. Metoden `ToLower()` bruker standardregler for å gjøre konverteringen, men disse reglene kan variere basert på hvilket språk som er satt som standard på datamaskinen. Dette kan være viktig å være klar over hvis du jobber med flerspråklige applikasjoner.

En annen ting å merke seg er at metoden `ToLower()` returnerer en ny streng med de konverterte bokstavene, og endrer ikke den opprinnelige strengen. Dette gjør det til en tryggere metode å bruke, siden du ikke risikerer å endre den opprinnelige dataen.

## Se også

- [MSDN - String.ToLower() Metode](https://docs.microsoft.com/nb-no/dotnet/api/system.string.tolower)
- [C# String Methods](https://www.tutorialspoint.com/csharp/csharp_string_methods.htm)
- [C# Lowercase Converter](https://www.c-sharpcorner.com/blogs/lowercase-string-converter-in-c-sharp1)