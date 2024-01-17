---
title:                "Alirivien erottaminen"
html_title:           "C#: Alirivien erottaminen"
simple_title:         "Alirivien erottaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Mikä on substringien erottelu ja miksi ohjelmoijat tekevät sitä? Substringien erottelu tarkoittaa merkkijonojen tietyn osan erottelua merkkijonoista. Tämä tehdään yleensä tietyn datan etsimiseksi ja käsittelemiseksi. Esimerkiksi voit erottaa sähköpostiosoitteen tietyn osan, kuten käyttäjänimen, ja käyttää sitä tietokannassa tai lähettää viestiä.

## Kuinka tehdä?
Tässä on yksinkertainen esimerkki substringien erottelusta C# -ohjelmointikielellä:

```C#
string s = "Tämä on esimerkki merkkijonosta";
string sub = s.Substring(5, 8); //5 on aloituskohdan indeksi ja 8 on substringin pituus
Console.WriteLine(sub); //tulostaa "on esime"
```

Saatu tuloste riippuu päätettiin aloituskohdan ja substringin pituuden arvoista. Voit myös käyttää muita substringien erottelufunktioita, kuten `Split` tai `Regex.Match`, riippuen tarpeistasi.

## Syvemmälle
Substringien erottelu ei ole jotain uutta, vaan se on ollut käytössä jo pitkään. Alun perin se tehtiin manuaalisesti, mutta nykyään on olemassa monia kirjastoja ja valmiita toimintoja, jotka helpottavat substringien erottelua. Voit myös käyttää muita tekniikoita, kuten regular expression, erottamaan tietynlaisia substringejä.

Joskus substringien erottelu voi aiheuttaa haasteita, etenkin jos merkkijonossa on monimutkaisia merkkejä tai koodaustapoja. Tässä tapauksessa kannattaa tarkistaa eri kirjastojen ja toimintojen dokumentaatio, jotta löydät parhaan ratkaisun tarpeisiisi.

## Katso myös
- [Substringien erottelu C# -dokumentaatiossa] (https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
- [Regex.Match-funktio C# -dokumentaatiossa] (https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.match?view=net-5.0)
- [Regular expressions 101 -sivusto] (https://regex101.com/), joka tarjoaa interaktiivisen tavan harjoitella regular expressionien käyttöä