---
title:                "Konvertere en streng til små bokstaver"
html_title:           "C#: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig når du jobber med tekstbehandling i et program. Dette gjør det enklere å matche og sammenligne tekster, uavhengig av om de er skrevet i store eller små bokstaver.

# Slik gjør du det

```C#
string tekst = "DETTE ER EN TEST";
string konvertertTekst = tekst.ToLower();
Console.WriteLine(konvertertTekst);
```

Dette vil gi følgende utskrift:

```
dette er en test
```

I dette eksempelet bruker vi metoden `ToLower()` som finnes på strengobjekter i C#. Denne metoden konverterer alle bokstavene i strengen til små bokstaver.

# Dykk dypere

I tillegg til å bruke `ToLower()` for å konvertere en hel streng til små bokstaver, kan du også bruke metoden `ToLowerInvariant()` for å sikre at konverteringen skjer på en konsistent måte uavhengig av systemets kulturinnstillinger.

Det er også verdt å nevne at metoden `ToLower()` ikke påvirker eventuelle tall eller andre tegn som ikke er bokstaver i strengen. Disse forblir uendret etter konverteringen.

# Se også

- [Microsoft sin offisielle dokumentasjon om ToLower() metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [Enkel guide til C# programmering](https://www.freecodecamp.org/news/learn-c-sharp-in-one-hour-335ef6c34925/)
- [10 nyttige strengoperasjoner i C#](https://www.c-sharpcorner.com/article/10-must-know-string-operations-in-c-sharp/)