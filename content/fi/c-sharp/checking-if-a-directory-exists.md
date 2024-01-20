---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "C#: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tarkistaa, onko hakemisto olemassa, tarkoittaa sitä, että koodissamme teemme tarkastuksen, jos tietyllä polulla on jo olemassa oleva hakemisto. Tämä on välttämätöntä, koska sen avulla ohjelmistokehittäjät voivat välttää virheitä, kuten luodaan samoja hakemistoja tai käsitellään olemattomia hakemistoja.

## Kuinka se toimii:

Tässä on esimerkkikoodi, joka näyttää, kuinka tarkistaa, onko hakemisto olemassa, C# -kielen nykyversiota käyttäen.

```C#
using System.IO;

public class DirectoryChecker
{
    public static bool DoesDirectoryExist(string directoryPath)
    {
        return Directory.Exists(directoryPath);
    }
}
```

Jos haluat tarkistaa, onko hakemisto olemassa, voit tehdä seuraavat:

```C#
string directoryPath = @"C:\SomeDirectory";

if (DirectoryChecker.DoesDirectoryExist(directoryPath))
{
    System.Console.WriteLine("Directory exists.");
}
else
{
    System.Console.WriteLine("Directory does not exist.");
}
```

Output:

```
Directory does not exist.
```

Muuta yllä olevaa `directoryPath` -muuttujaa ja testaa eri hakemisto-polkuja.

## Syvä sukellus:

Directory.Exists -funktio on osa .NET Frameworkia Microsoftilta ja se on ollut olemassa jo pitkään. Alternatiiveja tälle toiminnolle on olemassa, kuten yritys avata tiedosto ja ottaa virhe kiinni, mutta tämä voi aiheuttaa suorituskyvyn hitautta.

Funktion toteutus riippuu myös käytetystä käyttöjärjestelmästä. Windowsissa toiminto käyttää Win32 API -toimintoa nimeltä `GetFileAttributesW`. Unix-pohjaisissa järjestelmissä se käyttää `stat64` -toimintoa.

## Katso myös:

- Microsoftin virallinen dokumentaatio [System.IO.Directory.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- Stack Overflow keskustelu eri tavoista tarkistaa, onko hakemisto olemassa: [C# Directory.Exists alternatives](https://stackoverflow.com/questions/1395205/better-way-to-check-if-a-path-is-a-folder-or-a-file)