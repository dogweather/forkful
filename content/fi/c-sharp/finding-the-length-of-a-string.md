---
title:    "C#: Merkkijonon pituuden löytäminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit löytää merkkijonon pituuden? Lyhyesti sanottuna, merkkijonojen pituuden löytäminen on tärkeää, kun haluat käsitellä ja manipuloida tekstejä ohjelmoinnissa. Se on erityisen hyödyllistä esimerkiksi silloin, kun haluat tarkistaa, että käyttäjän syöttämä merkkijono on oikean pituinen ennen sen tallentamista tietokantaan.

## Kuinka

Löytääksesi merkkijonon pituuden C# -kielellä, sinun täytyy käyttää "Length" metodia, joka on perinteisesti käytössä kaikissa tiettyihin tietorakenteisiin liittyvissä toiminnoissa. Alla olevassa esimerkissä näytämme, kuinka voit käyttää "Length" -metodia ja tulostaa merkkijonon pituuden konsoliin:

```C#
// Luodaan merkkijono
string teksti = "Tämä on esimerkki";
// Käytetään "Length" metodia ja tulostetaan pituus konsoliin
Console.WriteLine("Merkkijonon pituus on: " + teksti.Length);
// Tulos: Merkkijonon pituus on: 18
```

Huomaa, että "Length" metodi palauttaa kokonaislukuna merkkijonon pituuden, joten voit käyttää tätä tietoa myös muihin toimintoihin koodissasi.

## Syväsukellus

Mikä tekee "Length" metodista niin kätevän merkkijonojen pituuden löytämisessä? "Length" -metodi laskee merkkien määrän merkkijonossa ja palauttaa sen kokonaislukuna, jonka avulla voit käsitellä merkkijonoa haluamallasi tavalla. Lisäksi "Length" metodi toimii myös muissa tietorakenteissa, kuten listoissa ja taulukoissa, joten voit käyttää sitä moniin eri tarkoituksiin.

Toinen tärkeä huomio on, että "Length" metodi laskee myös välilyönnit ja muut erikoismerkit merkkijonossa, joten voit olla varma, että saat oikean pituisen tuloksen.

## Katso myös

- C# merkkijonojen käsittely: https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0
- Merkkijonon pituuden selvittäminen C#: https://www.codegrepper.com/code-examples/delphi/c%23+string+length
- "Length" metodin käyttö taulukoissa ja listoissa: https://www.tutorialspoint.com/csharp/csharp_string_length.htm