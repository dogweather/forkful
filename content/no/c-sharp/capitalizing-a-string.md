---
title:                "C#: Stor bokstav i en streng"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne kapitalisere en streng, eller gjøre den om til store bokstaver, er en viktig ferdighet i programmering. Det kan være nyttig når du jobber med data eller skriver ut tekst på skjermen.

## Hvordan

Å kapitalisere en streng i C# er enkelt. Alt du trenger å gjøre er å bruke "ToUpper" metoden på strengen din. Se kodeblokken nedenfor for et eksempel:

```C#
string tekst = "dette er en test";
string kapitalisertTekst = tekst.ToUpper();
Console.WriteLine(kapitalisertTekst);
```

Output:

```
DETTE ER EN TEST
```

Det er viktig å merke seg at "ToUpper" metoden vil ikke bare endre alle små bokstaver til store bokstaver, men den vil også ignorere alle tegn som ikke er bokstaver, som for eksempel tall eller spesialtegn.

## Deep Dive

Nå som du vet hvordan du kan kapitalisere en streng, la oss se på noen flere ting å huske på når du bruker denne metoden. For det første vil "ToUpper" metoden bare endre bokstavene til store bokstaver i følge standard ASCII-tabellen. Dette betyr at bokstaver fra andre språk, som for eksempel "å", "ø" og "æ", ikke vil bli endret til sine tilsvarende store bokstaver.

Hvis du trenger å kapitalisere en streng som inneholder disse bokstavene, kan du bruke "ToUpperInvariant" metoden i stedet. Dette vil bruke en annen regel for å endre bokstavene til store bokstaver, som vil ta hensyn til språket som brukes i operativsystemet ditt.

En annen ting å huske på er at "ToUpper" metoden vil skape en ny streng, i stedet for å endre den eksisterende strengen. Dette betyr at hvis du vil lagre den kapitaliserte versjonen av strengen, må du tilordne den til en ny variabel.

## Se Også

- [string.ToUpper metode dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)
- [Hvordan formaterer du tekst i C#](https://www.codecademy.com/learn/learn-c-sharp/modules/learn-c-sharp-strings/cheatsheet)
- [En guide til ASCII-tabellen](https://www.ascii-code.com/)