---
title:                "Merkkijonojen yhdistäminen"
html_title:           "C#: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

IHminen saattaa haluta yhdistää merkkijonoja yhteen, jotta hän pystyy luomaan uusia merkkijonoja, jotka sisältävät tietokoneen käsittämää tietoa.

## Miten

Yhdistäminen ei ole monimutkaista! Käytä C#'n String.Concat-metodia.

```C#
// Luodaan kaksi merkkijonoa
string s1 = "Terve";
string s2 = "maailma";

// Yhdistetään merkkijonot
string s3 = String.Concat(s1, s2);

// Tulostetaan uusi merkkijono
Console.WriteLine(s3);

// Tulostus: Terve maailma
```

## Deep Dive

Yhdistäminen on tärkeä osa tietokoneohjelmointia, sillä se mahdollistaa erilaisten tietojen yhdistämisen ja käsittelyn. Tässä C#-esimerkissä käytimme String-luokan Concat-metodia, joka yhdistää kaksi merkkijonoa ja luo uuden merkkijonon. Tämä metodi on hyödyllinen erityisesti silloin, kun halutaan luoda käyttäjälle ymmärrettävä merkkijono esimerkiksi tulostuksen yhteydessä.

## Katso myös

- [C# - Merkkijonot](https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/strings/)
- [C# - String.Concat-metodi](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.concat?view=netframework-4.8)