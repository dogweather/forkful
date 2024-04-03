---
date: 2024-01-27 20:33:02.273957-07:00
description: "Kuinka: Yleisin tapa tuottaa satunnaislukuja C#-kielell\xE4 on k\xE4\
  ytt\xE4m\xE4ll\xE4 `System.Random`-luokkaa. T\xE4ss\xE4 on yksinkertainen esimerkki\
  \ sen k\xE4yt\xF6st\xE4."
lastmod: '2024-03-13T22:44:56.569103-06:00'
model: gpt-4-0125-preview
summary: "Yleisin tapa tuottaa satunnaislukuja C#-kielell\xE4 on k\xE4ytt\xE4m\xE4\
  ll\xE4 `System.Random`-luokkaa."
title: Satunnaislukujen generointi
weight: 12
---

## Kuinka:
Yleisin tapa tuottaa satunnaislukuja C#-kielellä on käyttämällä `System.Random`-luokkaa. Tässä on yksinkertainen esimerkki sen käytöstä:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Tuottaa numeron välillä 1 ja 99
        Console.WriteLine($"Satunnaisluku: {randomNumber}");
    }
}
```

Tämä tulostaa satunnaisen luvun, kuten:

```
Satunnaisluku: 42
```

Jos haluat tuottaa satunnaisen liukuluvun välillä 0,0 ja 1,0, voit käyttää `NextDouble`-metodia:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Satunnainen double: {randomDouble}");
```

Jos työskentelet turvallisuusherkän sovelluksen parissa, joka vaatii kryptografista satunnaisuutta, on parempi käyttää `System.Security.Cryptography`-löytyvää `RNGCryptoServiceProvider`-luokkaa:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Luo 4-bittisen pitkän satunnaisluvun
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Kryptografisesti turvallinen satunnaisluku: {value}");
    }
}
```

## Syväsukellus
Satunnaislukujen tuottaminen C#-kielellä on kehittynyt vuosien varrella. Alun perin `System.Random`-luokka oli se menetelmä, joka oli valittu pseudosatunnaislukujen generointiin. Se on pseudosatunnainen, koska tietyllä siemenarvolla se tuottaa saman numerosarjan, mikä voi olla hyödyllistä virheenkorjauksessa tai testien toistettavuudessa.

Vaikka se riittää perustarpeisiin, `System.Random` ei ole säieturvallinen ja voi tuottaa ennakoitavia tuloksia, mikä ei sovellu turvallisuudesta riippuvaisiin sovelluksiin. Tämä rajoitus johti `RNGCryptoServiceProvider`-luokan esittelyyn kryptografista satunnaisuutta varten, joka on turvallisempi mutta myös resurssi-intensiivisempi.

Vaihtoehtona .NET Corelle ja .NET 5+:lle on `RandomNumberGenerator`-luokka `System.Security.Cryptography`-kirjastossa turvallisten satunnaislukujen generointiin, joka on tarkoitettu modernimmaksi ja helpommaksi vaihtoehdoksi verrattuna `RNGCryptoServiceProvider`-luokkaan.

Jokaisella satunnaislukujen tuottamisen menetelmällä C#-kielellä on paikkansa sovelluksen vaatimusten mukaan. Useimmissa sovelluksissa `System.Random` riittää, mutta niihin, jotka vaativat turvallisia, ennustamattomia satunnaislukuja, kryptografiset luokat tarjoavat vankan vaihtoehdon.
