---
title:                "Tekstitiedoston kirjoittaminen"
aliases:
- fi/c-sharp/writing-a-text-file.md
date:                  2024-02-03T19:27:41.525022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekstitiedoston kirjoittaminen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tekstitiedoston kirjoittaminen C#:ssa käsittää ohjelmallisesti tekstiedostojen luomisen tai muokkaamisen tiedostojärjestelmässä - tämä on perustehtävä monille sovelluksille, kuten lokitiedostojen kirjoittaminen, datan vienti tai konfiguraation hallinta. Ohjelmoijat suorittavat tämän toimenpiteen tallentaakseen dataa istuntojen välillä, jakaakseen tietoa järjestelmien kesken tai tallentaakseen ihmisen luettavaa tulostetta.

## Miten:
C# yksinkertaistaa tiedosto-operaatioita `System.IO` nimiavaruuden avulla, tarjoten suoraviivaisia metodeja tekstiedostojen kirjoittamiseen. Tässä on, miten kirjoitetaan perustekstitiedosto ja lisätään tekstiä olemassa olevaan tiedostoon.

### Kirjoittaminen tyhjästä tekstiedostoon
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Hei maailma!";

        // Kirjoita sisältö uuteen tiedostoon
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("Tiedosto kirjoitettu onnistuneesti.");
    }
}
```
**Esimerkkituloste:**
```
Tiedosto kirjoitettu onnistuneesti.
```

### Tekstin lisääminen olemassa olevaan tiedostoon
Jos haluat lisätä tekstiä olemassa olevan tiedoston loppuun, voit käyttää `File.AppendAllText` -metodia.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string lisäsisältö = "\nLisätään lisää sisältöä.";

        // Lisää sisältöä tiedostoon
        File.AppendAllText(filePath, lisäsisältö);
        
        Console.WriteLine("Sisältö lisätty onnistuneesti.");
    }
}
```
**Esimerkkituloste:**
```
Sisältö lisätty onnistuneesti.
```

### Kolmannen osapuolen kirjastojen käyttö: `StreamWriter`
Hienojakoisemman kirjoitusvalvonnan saamiseksi, mukaan lukien automaattinen tyhjennys ja koodausten valinta, käytä `StreamWriter`ia.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string sisältö = "Tämä on esimerkki käyttäen StreamWriteria.";

        // Käytetään StreamWriteria kirjoittaaksemme tiedostoon
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(sisältö);
        }
        
        Console.WriteLine("Tiedosto kirjoitettu StreamWriterin kanssa onnistuneesti.");
    }
}
```
**Esimerkkituloste:**
```
Tiedosto kirjoitettu StreamWriterin kanssa onnistuneesti.
```

Jokainen näistä lähestymistavoista palvelee erilaisia tarpeita: suorat `File` metodit nopeisiin operaatioihin ja `StreamWriter` monimutkaisempiin kirjoitusskenaarioihin. Valitse tarpeidesi mukaan, ottaen huomioon tekijöitä kuten suorituskyky ja tiedostokoko.
