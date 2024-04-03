---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:41.525022-07:00
description: "Tekstitiedoston kirjoittaminen C#:ssa k\xE4sitt\xE4\xE4 ohjelmallisesti\
  \ tekstiedostojen luomisen tai muokkaamisen tiedostoj\xE4rjestelm\xE4ss\xE4 - t\xE4\
  m\xE4 on perusteht\xE4v\xE4\u2026"
lastmod: '2024-03-13T22:44:56.592393-06:00'
model: gpt-4-0125-preview
summary: "Tekstitiedoston kirjoittaminen C#:ssa k\xE4sitt\xE4\xE4 ohjelmallisesti\
  \ tekstiedostojen luomisen tai muokkaamisen tiedostoj\xE4rjestelm\xE4ss\xE4 - t\xE4\
  m\xE4 on perusteht\xE4v\xE4 monille sovelluksille, kuten lokitiedostojen kirjoittaminen,\
  \ datan vienti tai konfiguraation hallinta."
title: Tekstitiedoston kirjoittaminen
weight: 24
---

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
