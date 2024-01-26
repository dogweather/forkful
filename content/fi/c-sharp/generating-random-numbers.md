---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:48:47.152560-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen generointi tarkoittaa ennustamattomien lukujen tuottamista algoritmien avulla. Ohjelmoijat käyttävät niitä, kun tarvitaan elementtiä sattumanvaraisuudesta – olipa kyseessä sitten pelin arvaamattomuus tai tietoturvan parantaminen.

## Näin teet:
```C#
using System;

class SatunnaislukuDemo
{
    static void Main()
    {
        // Luo Random-olio
        Random r = new Random();

        // Arvo satunnaisluku 1 ja 100 välillä
        int satunnaisluku = r.Next(1, 101);
        Console.WriteLine(satunnaisluku);
        
        // Arvo toinen satunnainen desimaaliluku
        double satunnaisdesimaali = r.NextDouble();
        Console.WriteLine(satunnaisdesimaali);
    }
}
```
Esimerkkituloste voisi olla:
```
42
0.8471847
```

## Syväsukellus
Algoritmit satunnaislukujen generointiin ovat yleistyneet 1940-luvulta lähtien. `System.Random` on standardi C#-kirjasto random-numeroiden luontiin, mutta se ei ole kryptografisesti turvallinen. Kryptografisiin tarkoituksiin kannattaa käyttää `System.Security.Cryptography` -namespacesta löytyviä luokkia, kuten `RNGCryptoServiceProvider`. `Random` käyttää ns. pseudosatunnaisuutta – algoritmi tarvitsee "siemenarvon" (`seed`), joka vaikuttaa generoituihin numeroihin. Jos siemen on sama, samat luvut generoidaan uudelleen.

## Katso myös
- Microsoftin dokumentaatio `Random`-luokasta: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- `RNGCryptoServiceProvider` esimerkkejä ja käyttö: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider)
