---
title:                "C#: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en streng til store bokstaver kan være en svært nyttig funksjon i mange programmeringssammenhenger. Det kan hjelpe med å gjøre data mer leselige og organiserte, og det kan også være en del av en større databehandling eller formateringsprosess.

## Hvordan

Å konvertere en streng til store bokstaver i C# er en enkel prosess som kan gjøres ved hjelp av en innebygd funksjon kalt "ToUpper()". Denne funksjonen tar inn en streng som argument og returnerer en kopi av strengen med alle bokstavene omgjort til store bokstaver. La oss se på et eksempel:

```C#
string navn = "oskar";
string storNavn = navn.ToUpper();
Console.WriteLine(storNavn);
```

Dette koden vil skrive ut "OSKAR" til konsollen. Som du kan se, er det bare å kalle på funksjonen "ToUpper()" på strengen du ønsker å konvertere.

## Deep Dive

For de som er interessert i å gå dypere inn i å konvertere strenger til store bokstaver, er det viktig å vite at denne funksjonen kun konverterer bokstaver som er en del av det engelske alfabetet. Dette betyr at bokstaver med aksenter eller andre spesielle symboler ikke vil bli omgjort, med mindre de er en del av det engelske alfabetet. Det er også verdt å merke seg at denne funksjonen ikke påvirker tall eller andre tegn i en streng.

Det finnes også alternative måter å konvertere en streng til store bokstaver på i C#, som å bruke funksjonen "ToLower()" for å omgjøre alle bokstavene til små bokstaver. Det finnes også måter å gjøre dette på ved å bruke "CultureInfo" og "TextInfo" klassene for å håndtere spesifikke kulturavhengige konverteringer.

## Se Også

For mer informasjon om konvertering av strenger i C#, kan du sjekke ut disse nyttige lenkene:

- [Microsofts offisielle dokumentasjon om "ToUpper()" funksjonen](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [En guide til kulturavhengig tekstbehandling i C#](https://www.c-sharpcorner.com/UploadFile/a74768/culture-dependent-text-info-functions/)
- [En diskusjon om forskjellene mellom "ToUpper()" og "ToUpperInvariant()" funksjonene](https://stackoverflow.com/questions/4930859/difference-between-toupper-and-toupperinvariant-in-net)