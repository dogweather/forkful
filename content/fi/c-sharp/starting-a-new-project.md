---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Aloittaminen uudella projektilla C#-ohjelmoinnissa

## Mikä & Miksi?

Aloittaminen uudella projektilla tarkoittaa ohjelman luomisen alkupistettä - tyhjältä pöydältä kohti lopullista sovellusta. Ohjelmoijat tekevät niin, koska se on tapa ratkaista uusi ongelma tai luoda uusi työkalu.

## Kuinka:

Tässä on yksinkertainen esimerkki C# -projektin luomisesta käyttäen .NET Core -ympäristöä. Projektin nimi on "HelloWorld":

```
dotnet new console -o HelloWorld
cd HelloWorld
```

Kun avaat `Program.cs` -tiedoston, näet jotain tällaista:

```C#
using System;

namespace HelloWorld
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello, World!");
        }
    }
}
```

Kun ajat ohjelman, se tulostaa "Hello, World!".

```
dotnet run
```

```Output
Hello, World!
```

## Syvällisempi tarkastelu

.NET Core on Microsoftin vapaa ja avoimen lähdekoodin kehitysympäristö aloittaa uusia C# -projekteja. Vaikka .NET Core -projektin aloittaminen `dotnet new` -komennolla on yleisin tapa, on myös muita keinoja kuten Visual Studio IDE:n käyttö. 

Historiallisessa kontekstissa, alun perin uuden projektin aloittaminen C#:ssa merkitsi monimutkaista prosessia, johon kuului asennettavien tehtäväpohjien määrittämistä ja konfigurointia. Nykyään .NET Core ja modernit IDE:t tekevät prosessista paljon yksinkertaisemman.

## Katso myös

1. Microsoftin virallinen ohje aloittamiseksi uuden .NET Core -projektin kanssa: [https://docs.microsoft.com/en-us/dotnet/core/tutorials/with-visual-studio](https://docs.microsoft.com/en-us/dotnet/core/tutorials/with-visual-studio).
2. Visual Studio 2019:n asennusohjeet: [https://visualstudio.microsoft.com/vs/](https://visualstudio.microsoft.com/vs/).
3. StackOverflow-keskustelu projektipohjien ja räätälöityjen mallien määrittämisestä .NET Core:ssa: [https://stackoverflow.com/questions/42505396/do-net-core-projects-have-project-templates](https://stackoverflow.com/questions/42505396/do-net-core-projects-have-project-templates).