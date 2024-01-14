---
title:    "C#: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

### Miksi muuttaa päivämäärä merkkijonoksi?

Päivämäärien muuttaminen merkkijonoiksi on tärkeä osa ohjelmointia, joka mahdollistaa päivämääräarvojen tallentamisen ja käsittelyn tietokannoissa ja tekstitiedostoissa. Merkkijonoiksi muuttaminen myös helpottaa päivämääräarvojen tulkintaa ja näyttämistä käyttäjille.

### Kuinka muuttaa päivämäärä merkkijonoksi?

```C#
// Alustetaan DateTime-tyyppinen muuttuja, joka sisältää nykyisen päivämäärän.
DateTime tänään = DateTime.Today;
// Muutetaan päivämäärä merkkijonoksi käyttämällä ToString-metodia.
string merkkijonoPäivästä = tänään.ToString();
// Tulostetaan merkkijono konsoliin.
Console.WriteLine(merkkijonoPäivästä);
```

**Tulos:**

```
02/10/2021 00:00:00
```

### Syventävä tarkastelu

Päivämäärän muuttaminen merkkijonoksi ei ole aina yksiselitteinen prosessi, vaan siihen liittyy erilaisia muotoilu- ja lokalisaatio-ongelmia. Esimerkiksi koodin pitäisi tuottaa eri formaattia eri kielissä. Lisäksi merkkijonon sisältö ja muotoilu voivat vaihdella sen mukaan, onko päivämäärä esimerkiksi ennen vai jälkeen puolenyön.

C# tarjoaa kuitenkin laajan valikoiman erilaisia muotoiluvaihtoehtoja päivämäärän muuttamiseen merkkijonoksi, kuten päivämäärän esittäminen tiettynä viikonpäivänä tai vuoden päivämäärän järjestysnumerona.

### Katso myös

- [DateTime.ToString Method (System) - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
- [Standard Numeric Format Strings - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-numeric-format-strings?view=net-5.0)