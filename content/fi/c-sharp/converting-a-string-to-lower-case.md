---
title:                "C#: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pienaakkosiksi? Pienaakkosten käyttö mahdollistaa tekstin yhdenmukaistamisen ja helpottaa vertailua muihin merkkijonoihin.

## Kuinka tehdä se

Käytä C# -ohjelmointikielen string-luokan ToLower-metodia muuttaaksesi merkkijonon pienaakkosiksi. Tässä on esimerkki ja siihen liittyvä tulostus:

```C#
String str = "PÄIVÄN SÄÄ ON AURINKOINEN";
String lowerStr = str.ToLower();

Console.WriteLine(lowerStr);
```

Tämän koodin tulos on:

```
päivän sää on aurinkoinen
```

## Syvällinen katsaus

Kun käytät ToLower-metodia muuttaaksesi merkkijonon pienaakkosiksi, C# käyttää käyttöjärjestelmän oletusarvoista kielimuunnosta. Tämä tarkoittaa sitä, että tulokseksi saattaa tulla merkkijono, joka ei ole odotetussa muodossa. Voit lisätä koodiisi kieliuudelleenmäärityksen, joka varmistaa, että merkkijonon pienaakkoset ovat oikeassa muodossa.

## Katso myös

- [C# string-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
- [C# string-luokan ToLower-metodin dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [Kielimuunnoksen määrittäminen C#:ssa](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0)