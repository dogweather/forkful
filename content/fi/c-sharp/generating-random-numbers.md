---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Satunnaislukujen tuottaminen C#-ohjelmointikielellä 

## Miksi & Mitä?
Satunnaislukujen luominen tarkoittaa arvaamattomien numeroiden generointia. Ohjelmoijat tekevät tämän simuloidakseen sattumanvaraisuutta ohjelmissa, esimerkiksi arpoessaan palkintoja tai luodessaan testidataa.

## Näin teet:
Voit luoda satunnaislukuja C#-kielellä käyttämällä `Random`-luokkaa. Alla on esimerkki:

```C#
using System;

class Program
{
    static void Main()
    {
        Random r = new Random();
        int x = r.Next(0, 100);
        Console.WriteLine("Satunnainen luku väliltä 0-100: " + x);
    }
}
```
Kun suoritat tämän koodin, saat tulostuksena satunnaisen numeron väliltä 0-100.

## Syväsukellus:
**Historiallinen Konteksti:** Satunnaislukujen generointi on ollut tietokoneohjelmoinnin keskeinen osa jo varhaisista päivistä lähtien. Alun perin tämä liittyi pääasiassa peliohjelmointiin ja kryptografiaan.

**Vaihtoehdot:** `Random`-luokan lisäksi voit käyttää `RNGCryptoServiceProvider`-luokkaa luomaan krypto- vahvistettuja satunnaislukuja.

**Toteutuksen yksityiskohdat:** `Random`-luokka perustuu pseudo-satunnaisgeneraattorin algoritmiin, joka tuottaa pitkän sarjan numeroita, jotka näyttävät olevan satunnaisia ja arvaamattomia.

## Katso myös:
- [Microsoftin virallinen dokumentaatio `Random`-luokasta](https://docs.microsoft.com/fi-fi/dotnet/api/system.random?view=net-5.0)
- [Ohjelmoijan pikaopas generaattoreiden käyttöön](https://blog.submain.com/guide-random-number-generators-csharp/)
- [Microsoftin artikkeli satunnaislukujen tuottamisesta krypto- vahvistetulla tavalla](https://docs.microsoft.com/fi-fi/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)