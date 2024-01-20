---
title:                "Sjekke om en mappe finnes"
html_title:           "Arduino: Sjekke om en mappe finnes"
simple_title:         "Sjekke om en mappe finnes"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Å sjekke om en katalog eksisterer handler om å bekrefte at en bestemt mappe er tilgjengelig på filsystemet før du prøver å lese fra eller skrive til den. Dette forhindrer feil som oppstår når kode antar at en mappe finnes, og den faktisk ikke gjør det.

## How to:

For å sjekke om en katalog eksisterer i C#, bruk `Directory.Exists()` metoden fra `System.IO`-navneområdet. Se på eksempelet:

```C#
using System;
using System.IO;

class DirectoryCheck
{
    static void Main()
    {
        string path = @"C:\EksempelKatalog";

        if(Directory.Exists(path))
        {
            Console.WriteLine($"Katalogen {path} eksisterer.");
        }
        else
        {
            Console.WriteLine($"Katalogen {path} eksisterer ikke.");
        }
    }
}
```

Om katalogen finnes, får du output:

```
Katalogen C:\EksempelKatalog eksisterer.
```

Ellers:

```
Katalogen C:\EksempelKatalog eksisterer ikke.
```

## Deep Dive

Å sjekke om en katalog eksisterer er en vesentlig operasjon datamaskiner har gjort siden de fikk et filsystem. I eldre programmeringsspråk var det oftere nødvendig å håndtere filsystemfeil direkte. I C#, derimot, tar `System.IO`-klassene seg av mye av den kompleksiteten for oss.

Alternativene inkluderer å prøve å lese eller skrive til en katalog og håndtere eventuelle unntak som en 'FileNotFoundException'. Men det er generelt sett dårlig praksis – det er bedre å bruke `Directory.Exists()` for eksplicitte sjekker før du prøver operasjoner på katalogen.

I bakhånd håndterer `Directory.Exists()` et par viktige sjekker for oss: det ser ikke bare at stien peker til noe som eksisterer, men det bekrefter også at dette noe er en katalog, ikke en fil.

## See Also

- Microsofts offisielle dokumentasjon for Directory.Exists metoden: [Directory.Exists Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- Guide til System.IO-navneområdet: [System.IO Namespace](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- Feilhåndtering i C#: [Exception Handling](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)