---
title:    "C#: Lese en tekstfil"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil kan være en viktig del av å utvikle et program. Det kan gi oss tilgang til et stort antall data som kan hjelpe oss med å analysere og manipulere informasjon. Å kunne lese og behandle tekstfiler effektivt er en essensiell ferdighet for enhver programmerer.

## Hvordan

For å lese en tekstfil i C#, kan vi bruke klassen `StreamReader` og dens innebygde funksjoner. Først må vi åpne filen ved å gi filbanen som parameter til konstruktøren til `StreamReader`. Deretter kan vi bruke funksjonen `ReadLine()` til å lese en linje av teksten. Vi kan også bruke funksjoner som `Peek()` for å se på neste tegn i filen eller `ReadToEnd()` for å lese hele teksten på en gang. Her er et eksempel på hvordan dette kan se ut i praksis:

```C#
using System;
using System.IO;

class TextFileReader
{
    static void Main()
    {
        // Åpner filen og oppretter en StreamReader
        StreamReader reader = new StreamReader(@"C:\eksempeltekstfil.txt");

        // Leser og skriver ut hver linje i tekstfilen
        while (!reader.EndOfStream)
        {
            string line = reader.ReadLine();
            Console.WriteLine(line);
        }

        // Lukker StreamReader når vi er ferdige
        reader.Close();
    }
}
```

### Output:

```
Dette er en linje med tekst.
Dette er en annen linje med tekst.
```

## Dypdykk

Mens `StreamReader` er en effektiv måte å lese tekstfiler på, er det også noen problemstillinger å være klar over. For eksempel kan det være lurt å bruke `using`-blokk når du arbeider med `StreamReader` for å sikre at den blir lukket på riktig måte. Det kan også være viktig å være forsiktig med tegnsettet til tekstfilen, da det kan føre til feil når du leser teksten. Derfor kan det være lurt å bruke `Encoding`-klassen for å spesifisere tegnsettet når du åpner filen.

## Se Også

- [MSDN - StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [Tutorialspoint - C# Streams](https://www.tutorialspoint.com/csharp/csharp_streams.htm)
- [C# File Handling](https://www.c-sharpcorner.com/UploadFile/mahesh/file-handling-in-C-Sharp/)