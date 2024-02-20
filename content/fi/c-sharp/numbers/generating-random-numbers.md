---
date: 2024-01-27 20:33:02.273957-07:00
description: "Satunnaislukujen tuottaminen C#-kielell\xE4 k\xE4sitt\xE4\xE4 ennalta-arvaamattomien\
  \ numeeristen arvojen luomisen m\xE4\xE4ritellyll\xE4 v\xE4lill\xE4. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t n\xE4it\xE4\u2026"
lastmod: 2024-02-19 22:05:15.462391
model: gpt-4-0125-preview
summary: "Satunnaislukujen tuottaminen C#-kielell\xE4 k\xE4sitt\xE4\xE4 ennalta-arvaamattomien\
  \ numeeristen arvojen luomisen m\xE4\xE4ritellyll\xE4 v\xE4lill\xE4. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t n\xE4it\xE4\u2026"
title: Satunnaislukujen generointi
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen tuottaminen C#-kielellä käsittää ennalta-arvaamattomien numeeristen arvojen luomisen määritellyllä välillä. Ohjelmoijat käyttävät näitä menetelmiä toteuttaakseen ominaisuuksia, kuten kryptografia, simulaatiot ja pelit, joissa vaaditaan ennustamattomuutta tai todellisen maailman satunnaisuuden simulointia.

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
