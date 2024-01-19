---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen liittäminen on toiminto, jossa yhdistetään kaksi tai useampi merkkijono yhdeksi isommaksi merkkijonoksi. Ohjelmoijat tekevät tämän, kun heidän täytyy tuottaa pitkä merkkijono pienistä palasista.

## Kuinka:

```C#
// Luodaan aluksi kaksi merkkijonoa
string sana1 = "Hei, ";
string sana2 = "mitä kuuluu?";
  
// Yhdistetään merkkijonot
string tervehdys = sana1 + sana2;

// Tulostetaan yhdistetty merkkijono
Console.WriteLine(tervehdys);
```
Tämä koodinpätkä tuottaa outputin:
```
Hei, mitä kuuluu?
```

## Syvempi syvennys

Merkkijonojen liittämistä on käytetty ohjelmoinnin varhaisista päivistä asti. C#-kielessä on monia tapoja liittää merkkijonoja, mukaan lukien + -operaattorit, `String.Concat()`, `String.Format()`, `StringBuilder` sekä uudessa C# 6-versiossa esitelty interpoloitu merkkijono.

Vaikka + -operaattori on yksinkertaisin ja yleisimmin käytetty tapa liittää merkkijonoja, sen käyttö suurien merkkijonojen tai paljon liitäntöjä vaativissa tilanteissa voi olla heikko ratkaisu suorituskyvyn kannalta. `StringBuilder` tai `String.Concat()` ovat tehokkaampia vaihtoehtoja näissä tilanteissa.

## Katso myös

Lisätietoja merkkijonotyypeistä ja toimenpiteistä, kuten liittäminen, löydät [Microsoftin viralliselta .NET-dokumentaatiosta](https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/strings/).