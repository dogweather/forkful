---
title:                "C#: Pienentäminen ja suurentaminen"
simple_title:         "Pienentäminen ja suurentaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi:
Miksi kukaan haluaisi muuttaa merkkijonon kaikki kirjaimet ISOIKSI?

Vastaus on yksinkertainen - joskus on tarvetta muuttaa merkkijonoa eri muotoon. Tämä voi olla tarpeellista esimerkiksi tietokantakyselyiden tekemisessä, käyttäjän syötteen käsittelyssä tai yksinkertaisesti merkkijonon muotoilussa. Tässä blogikirjoituksessa opit, miten voit käyttää C#-kielen toimintoja muuttaaksesi merkkijonon kaikki kirjaimet isoiksi.

## Kuinka:
```C#
string merkkijono = "Tämä on esimerkki merkkijonosta.";

// Käytetään string-luokan ToUpper() -metodia muuttaaksesi merkkijonon kaikki kirjaimet isoiksi.
string uusiMerkkijono = merkkijono.ToUpper();

Console.WriteLine(uusiMerkkijono);
// Uusi merkkijono on "TÄMÄ ON ESIMERKKI MERKKIJONOSTA."
```

Kuten yllä olevasta koodiesimerkistä näet, merkkijonon kaikkien kirjainten muuttaminen isoiksi on erittäin helppoa C#-kielen string-luokan ToUpper() -metodilla. Voit käyttää tätä metodia mihin tahansa merkkijonoon, joka sisältää vähintään yhden kirjaimen.

```C#
string nimi = "Finnish Readers";

Console.WriteLine(nimi.ToUpper());
// Uusi merkkijono on "FINNISH READERS"
```

Mutta miten tämä toimii taustalla?

## Syvempi sukellus:
Kun käytät ToUpper() -metodia, C#-kieli muuttaa merkkijonon jokaisen kirjaimen ISOIKSI. Tämä perustuu siihen, että jokaiselle kirjaimelle on olemassa vastaava ISO-koodi. Esimerkiksi kirjaimelle "a" on olemassa kaksi eri ISO-koodia - "A" ja "a". Metodi tarkistaa jokaisen merkin ja korvaa sen ISO-koodilla, jos sellainen löytyy.

On myös hyvä huomata, että ToUpper() -metodi ei vaikuta merkkijonon alkuperäiseen muotoon, vaan se luo uuden merkkijonon, joka sisältää muutetut isot kirjaimet. Tämä on tärkeä huomioitava esimerkiksi silloin, kun käsittelet käyttäjän syöttämää tietoa ja haluat säilyttää alkuperäisen merkkijonon muodon.

## Katso myös:
- [String-luokka (C#-ohjelmointiopas)](https://docs.microsoft.com/fi-fi/dotnet/api/system.string?view=net-5.0)
- [ToUpper() -Metodi (C#-ohjelmointiopas)](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.toupper?view=net-5.0)