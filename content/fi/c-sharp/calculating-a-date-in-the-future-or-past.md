---
title:    "C#: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

Tulevaisuuden tai menneisyyden päivämäärän laskeminen voi olla hyödyllistä esimerkiksi kirjanpitoa tai suunnittelua varten.

## Miten

```C#
// Alustetaan nykyinen päivämäärä 
DateTime nykyinenPaivamaara = DateTime.Now;

// Lisätään 30 päivää nykyiseen päivämäärään
DateTime viidenPaivanPaasta = nykyinenPaivamaara.AddDays(30);

// Tulostetaan tulos
Console.WriteLine("Päivämääränä 30 päivän päästä on: " + viidenPaivanPaasta.ToString("dd/MM/yyyy"));
```

**Tulos:**

Päivämääränä 30 päivän päästä on: 22/06/2021

```C#
// Alustetaan nykyinen päivämäärä 
DateTime nykyinenPaivamaara = DateTime.Now;

// Vähennetään 10 päivää nykyisestä päivämäärästä
DateTime kymmenenPaivanPaasta = nykyinenPaivamaara.AddDays(-10);

// Tulostetaan tulos
Console.WriteLine("Päivämääränä 10 päivän päästä on: " + kymmenenPaivanPaasta.ToString("dd/MM/yyyy"));
```

**Tulos:**

Päivämääränä 10 päivän päästä on: 02/06/2021

## Syvällisempi sukellus

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen perustuu DateTime-luokan tarjoamiin metodeihin. Käyttämällä AddDays-metodia, voimme lisätä tai vähentää haluamamme määrän päiviä nykyisestä päivämäärästä. Muita hyödyllisiä metodeja voi löytää DateTime-luokan dokumentaatiosta.

## Katso myös

- [DateTime-luokka (MSDN)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [DateTime-luokan käyttö C# -sovelluksissa (C# Corner)](https://www.c-sharpcorner.com/UploadFile/syedshanu/datetime-class-c-sharp/)
- [Päivämäärät ja kellonajat C# -ohjelmoinnissa (Microsoft Learn)](https://docs.microsoft.com/fi-fi/learn/modules/datetime/)