---
title:                "C#: Satunnaislukujen luominen"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet koskaan ohjelmoinut, niin olet varmasti tarvinnut satunnaisia numeroita jossakin vaiheessa. Saatat esimerkiksi tarvita satunnaisen luvun pelissä tai haluta arpoa voittajan kilpailussa. Onneksi C#:n avulla todella helppo ja nopea tapa generoida satunnaisia numeroita, ja tässä blogikirjoituksessa käymme läpi miten se tapahtuu.

## Kuinka

On olemassa useita tapoja generoida satunnaisia lukuja C#:ssa. Yksi tapa on käyttää Random-luokkaa, joka tarjoaa useita erilaisia metodeja satunnaisten lukujen generoimiseksi. Esimerkiksi voit käyttää Next-metodia generoidaksesi satunnaisen kokonaisluvun väliltä 1-10 seuraavasti:

```C#
var rnd = new Random();
int randomLuku = rnd.Next(1, 11);
Console.WriteLine("Satunnainen luku väliltä 1-10: " + randomLuku);
```

Tämä koodinpätkä tulostaa esimerkiksi "Satunnainen luku väliltä 1-10: 7".

Voit myös generoida satunnaisen desimaaliluvun käyttämällä NextDouble-metodia:

```C#
double randomDesimaali = rnd.NextDouble();
Console.WriteLine("Satunnainen desimaaliluku: " + randomDesimaali);
```

Tämä koodinpätkä voi esimerkiksi tulostaa "Satunnainen desimaaliluku: 0.865832".

## Syväsukellus

Random-luokka käyttää sisäistä algoritmia, joka perustuu maailman tietokonejärjestelmien kelloihin ja joka luo uuden arvon jokaiselle kutsulle. Jos haluat syvempää ymmärrystä siitä, miten Random-luokka toimii taustalla, voit tutkia sitä tarkemmin dokumentaatiosta.

On myös tärkeää huomata, että Random-luokka ei ole täysin satunnainen, vaan sen luoma jono on alustettu arvoilla, jotka ovat ennakoitavissa. Jos tarvitset erittäin todennäköisesti satunnaisia lukuja, voit harkita toisen algoritmin, kuten RNGCryptoServiceProviderin käyttämistä.

## Katso myös

- [C#:n Random-luokan dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [RNGCryptoServiceProviderin käyttö C#:ssa](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)