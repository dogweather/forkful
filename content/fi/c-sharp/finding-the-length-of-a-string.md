---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Määritettäessä merkkijonon pituutta, määritämme kuinka monta merkkiä (tai sanaa, numeroa, symbolia jne.) merkkijonossa on. Ohjelmoijat tekevät tämän tietäen kuinka paljon tietoa tietty merkkijono sisältää tai käyttäen sitä ohjelman logiikassa, esimerkiksi tarkistaessaan syötteitä tai jaettaessa merkkijonoja osiin.

## Kuinka:

Käytämme C#-ohjelman `Length`-ominaisuutta merkkijonon pituuden määrittämiseen.

```C#
string phrase = "Hei, maailma!";
int length = phrase.Length;
Console.WriteLine("Merkkijonon pituus on: " + length);
```

Kun suoritat tämän koodin, saat tulosteen:

```C#
Merkkijonon pituus on: 14
```

## Syvä sukellus:

Historiallisesti merkkijonon pituuden määrittäminen on ollut yleinen toiminto, joka oli tarpeen jo varhaisesta tietojenkäsittelykaudesta lähtien. C# tarjoaa `Length`-attribuutin yksinkertaisten merkkijonojen mittaamiseen, mutta voit myös käyttää `StringInfo`-luokkaa, jos työskentelet yhdistettyjen merkkien tai monitavuisen merkkijonon kanssa.

```C#
string phrase = "Hei, 🌎!";
int lengthInChars = phrase.Length;
int lengthInTextElements = new System.Globalization.StringInfo(phrase).LengthInTextElements;

Console.WriteLine("Merkkijonon pituus merkkeinä: " + lengthInChars);
Console.WriteLine("Merkkijonon pituus tekstielementteinä: " + lengthInTextElements);
```

Tämä koodi tulostaa:

```C#
Merkkijonon pituus merkkeinä: 9
Merkkijonon pituus tekstielementteinä: 8
```

Tämä johtuu siitä, että joidenkin merkkien, kuten emojien, pituus on tavallisesti enemmän kuin yksi perinteisessä merkkijonojen esityksessä.

## Katso myös:

1. Microsoftin dokumentaatio merkkijonon pituudesta C#-kielessä: [Linkki](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
3. Miten laskea merkkijonojen pituus muilla ohjelmointikielillä: [Linkki](https://www.w3schools.com/jsref/jsref_length_string.asp)