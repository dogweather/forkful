---
title:                "Merkkijonon interpolointi"
aliases:
- fi/c-sharp/interpolating-a-string.md
date:                  2024-01-20T17:50:53.631203-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Merkkijonojen interpolointi tarkoittaa muuttujien, lausekkeiden ja funktioiden arvojen sijoittamista suoraan merkkijonoihin. Ohjelmoijat käyttävät tätä tuottaakseen helposti luettavia ja ylläpidettäviä viestejä tai tulosteita.

## How to:
```C#
// Käytetään merkkijonojen interpolointia muuttujien yhdistämiseen
string nimi = "Pekka";
int ika = 30;
string tervehdys = $"Hei, minun nimeni on {nimi} ja olen {ika} vuotta vanha.";

Console.WriteLine(tervehdys);
// Tuloste: Hei, minun nimeni on Pekka ja olen 30 vuotta vanha.
```

```C#
// Laskelmien teko suoraan interpoloidussa merkkijonossa
double hinta = 19.99;
double vero = 0.24;
string hintaviesti = $"Tuotteen loppuhinta veron kanssa on {hinta * (1 + vero):C}.";

Console.WriteLine(hintaviesti);
// Tuloste: Tuotteen loppuhinta veron kanssa on 24,79 €.
```

## Deep Dive
Merkkijonojen interpolointi otettiin käyttöön C#:ssa versiossa 6.0 ja sitä on paranneltu versio versiolta. Ennen interpolointia ohjelmoijat käyttivät `String.Format()`-metodia tai konkatenointia, mutta nämä olivat monimutkaisempia ja alttiimpia virheille.

Interpoloitu merkkijono käyttää `$`-merkkiä tunnistamaan, että merkkijonto sisältää interpolaatioita. Käyttämällä `{}` aaltosulkeita, voimme laittaa muuttujia ja lausekkeita suoraan merkkijonon sisään.

C#:n kompilaattori käsittelee interpoloidut merkkijonot sisäisesti kutsumalla `String.Format()`-metodia. Tämä tarkoittaa, että suorituskyky on yhteneväinen `String.Format()`-kutsujen kanssa, mutta koodi on selkeämpää.

Kun käytetään interpolointia suurissa projekteissa kannattaa kiinnittää huomiota siihen, miten interpolaatiot tehdään. Monien muuttujien ja monimutkaisten lausekkeiden kanssa koodi voi muuttua sekavaksi.

## See Also
- [Microsoftin dokumentaatio merkkijonojen interpoloinnista](https://docs.microsoft.com/fi-fi/dotnet/csharp/language-reference/tokens/interpolated)
- [C#-ohje: Merkkijonot](https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/strings/)
- [String.Format() metodi](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.format?view=netcore-3.1)
