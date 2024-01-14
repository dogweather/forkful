---
title:                "C#: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit löytää merkkijonon pituuden? Merkkijonot ovat olennainen osa ohjelmointia ja niitä käytetään usein tiedon tallentamiseen ja käsittelyyn. Merkkijonon pituuden selvittäminen voi auttaa sinua luomaan tehokkaampaa ja toimivampaa koodia.

## Miten

```C#
string s = "Tämä on esimerkkimerkkijono";
int length = s.Length;
Console.WriteLine(length);
```

Tässä esimerkissä olemme määrittäneet muuttujan "s" ja tallentaneet siihen merkkijonon arvon. Käytämme sitten merkkijonon "Length" ominaisuutta saadaksemme merkkijonon pituuden ja tallentamaan sen muuttujaan "length". Lopuksi tulostamme pituuden konsoliin.

```
Ulostulo:
27
```

Voit myös käyttää merkkijonon "Length" ominaisuutta suoraan tulostamalla sen ilman muuttujaa.

```C#
string s = "Tämä on esimerkkimerkkijono";
Console.WriteLine(s.Length);
```

```
Ulostulo:
27
```

## Syvällisempi sukellus

Merkkijonon "Length" ominaisuus palauttaa kokonaislukuarvon, joka edustaa merkkijonossa olevien merkkien määrää. Se ei sisällä lopetusmerkkiä, joten jos haluat käyttää sitä jatkoukselle, sinun tulee lisätä yksi lisäyksikkö.

Merkkijonon pituuden selvittäminen on tärkeää myös silloin, kun haluat tarkistaa, onko merkkijonossa tietty määrä merkkejä tai jos haluat helposti jakaa merkkijonon osiin sen pituuden perusteella.

## Katso myös

- [C# merkkijonot](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [Merkkijonon "Length" ominaisuus](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1)
- [Miten luoda merkkijonon pohjaisia sivuja C#:llä](https://www.c-sharpcorner.com/uploadfile/553aa9/how-to-create-string-based-web-page-in-C-Sharp/)