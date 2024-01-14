---
title:    "C#: Tiedostotekstin lukeminen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi lukisi teksti-tiedostoa C#-ohjelmoinnissa. Se voi olla hyödyllistä esimerkiksi kun haluat lukea ja käsitellä suuria määriä dataa tai kun haluat tallentaa käyttäjän syöttämiä tietoja.

## Kuinka tehdä

Lukeminen teksti-tiedostosta C# koodilla on helppoa. Seuraavassa on esimerkki koodista, joka lukee teksti-tiedoston ja tulostaa sen sisällön konsoliin.

```C#
// Avaaminen ja lukeminen tiedostosta
string tiedosto = @"polku/tiedosto.txt";
string sisalto = File.ReadAllText(tiedosto);

// Tulostaminen konsoliin
Console.WriteLine(sisalto);
```

### Tulostus
```
Tämä on esimerkki teksti-tiedostosta.
Siinä on muutama rivi tekstiä.
Voit lukea sen C# koodilla.
```

## Syvällinen sukellus

Teksti-tiedoston lukeminen ei rajoitu vain yllä olevaan esimerkkiin. Voit myös halutessasi käyttää lukemiseen teksti-tiedosto-olioita ja erilaisia lukemiseen tarkoitettuja funktioita, kuten `ReadLines()`.

Lisäksi voit myös käsitellä tiedostosta luetun datan haluamallasi tavalla, kuten tallentaa sen muuttujiin tai käyttää sitä osana muuta koodia.

## Katso myös

- [C# tiedoston luku ja kirjoitus](https://www.linkedin.com/pulse/lukeminen-ja-kirjoittaminen-c-koodissa-mikko-sprinkman)
- [C# tiedostojen käsittely](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-one-line-at-a-time)