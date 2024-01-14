---
title:    "C#: Tarkistetaan, onko hakemistoa olemassa"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat tarkistaa, onko hakemisto olemassa. Yleisimpiä ovat työnkulun automatisointi ja virheiden välttäminen, kun yrität päästä käsiksi tiettyyn hakemistoon.

## Miten

Tarkistamalla, onko hakemisto olemassa, voit välttää virheitä ja käsitellä niitä tarvittaessa. Voit tehdä tämän helposti C#-kielellä käyttämällä Directory.Exists()-metodia.

```
C#
string hakemistoPolku = "C:\\Users\\Kayttaja\\Tallennetut tiedostot";

if (Directory.Exists(hakemistoPolku))
{
    Console.WriteLine("Hakemisto on olemassa.");
}
else
{
    Console.WriteLine("Hakemistoa ei löytynyt.");
}
```

Tämä koodi tarkistaa, onko "Tallennetut tiedostot" -hakemisto olemassa käyttäjän "Kayttaja" kotihakemistossa. Jos hakemisto löytyy, tulostetaan "Hakemisto on olemassa.". Muussa tapauksessa tulostetaan "Hakemistoa ei löytynyt.". Tämä yksinkertainen tarkistus auttaa sinua välttämään virheitä ja parantamaan koodisi suorituskykyä.

## Syvällinen katsaus

Jos haluat tietää tarkemmin, miten Directory.Exists()-metodi toimii, voit tutkia sen toimintaa syvällisemmin. Teknisesti ottaen metodi tarkistaa, onko annetun polun tai hakemiston kansio olemassa. Se palauttaa boolean-arvon, joka kertoo, onko hakemisto olemassa vai ei. Tätä tietoa voit käyttää esimerkiksi päätöksentekoon tai virheiden käsittelyyn.

## Katso myös

- [Tietoja hakemistojen tarkistamisesta C#-kielellä](https://docs.microsoft.com/fi-fi/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Hakemistojen käsitteleminen C#-kielellä](https://www.tutorialspoint.com/csharp/csharp_directory_class.htm)
- [Vianjäljitysvinkkejä hakemistojen tarkistamiseen C#-kielellä](https://www.fluxbytes.com/csharp/check-if-a-directory-exists-using-csharp/)