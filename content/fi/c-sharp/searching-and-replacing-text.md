---
title:    "C#: Tekstin etsiminen ja korvaaminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Tervetuloa lukea blogia, joka kattaa suositun C# -ohjelmoinnin ja kuinka voit käyttää sitä tehokkaasti etsiäksesi ja korvataksesi tekstiä. Tekstintaulukon hakeminen ja korvaaminen on tärkeä osa ohjelmointia, joka säästää aikaa ja vaivaa manuaalisesta työstä. Tässä artikkelissa opit, miksi tekstintaulukon hakeminen ja korvaaminen on tärkeä, kuinka voit tehdä sen C# -ohjelmoinnissa ja syvällisempiä tietoja aiheesta.

## Miksi

Jokainen ohjelmoija kohtaa tilanteen, jossa hänen täytyy etsiä ja korvata tiettyä tekstiä koodissaan. Se voi olla virheellinen koodinpätkä, vanhentunut muuttuja tai yksinkertaisesti tarve muuttaa tiettyä sanaa useista kohdista koodia. Tällöin tekstintaulukon hakeminen ja korvaaminen C# -ohjelmoinnissa tulee tarpeeseen ja säästää paljon aikaa ja vaivaa.

## Kuinka

C# tarjoaa kätevän tavan etsiä ja korvata tekstiä koodissaan käyttämällä Replace-metodia. Tämä metodi etsii annetun merkkijonon ja korvaa sen toisella merkkijonolla. Seuraava koodinpätkä esittää, kuinka voit käyttää Replace-metodia:

```C#
string teksti = "Tämä teksti sisältää sanaa vanha";
string uusiTeksti = teksti.Replace("vanha", "uusi");
Console.WriteLine(uusiTeksti);
```

Tulos:

```
Tämä teksti sisältää sanaa uusi
```

Voit myös käyttää Replace-metodia haluamassasi tiedostossa. Seuraava koodinpätkä etsii ja korvaa tekstin tiedostossa "testi.txt" ja tallentaa uuden version "uusi.txt"-tiedostoon:

```C#
string tiedosto = "testi.txt";
string uusiTiedosto = "uusi.txt";
string teksti = File.ReadAllText(tiedosto);
string uusiTeksti = teksti.Replace("vanha", "uusi");
File.WriteAllText(uusiTiedosto, uusiTeksti);
```

Tämä on vain yksi esimerkki siitä, kuinka voit etsiä ja korvata tekstin C# -ohjelmoinnissa. On tärkeää muistaa, että Replace-metodi on korkean prioriteetin operaatio, joka voi hidastaa ohjelman suorituskykyä, jos sitä käytetään liian suurissa tiedostoissa tai usein. Muista aina optimoida koodisi ja käyttää Replace-metodia harkiten.

## Syvällinen sukellus

C# tarjoaa myös muita tapoja etsiä ja korvata tekstiä koodissa, kuten Regex-luokan avulla. Regex on lyhenne sanoista "regular expression" eli säännöllinen lauseke. Se tarjoaa monipuolisemman tavan etsiä ja korvata tekstiä, mutta vaatii hieman enemmän ohjelmointitaitoja.

Tärkeintä etsiessä ja korvattaessa tekstiä on ymmärtää, miten säännölliset lausekkeet toimivat. Ne määrittelevät tietyn kaavan, jota verrataan annettuun tekstiin. Seuraava koodinpätkä näyttää, kuinka voit etsiä ja korvata tekstiä käyttämällä säännöllisiä lausekkeita:

```C#
string teksti = "Tämä teksti sisältää sanaa vanha";
string uusiTeksti = Regex.Replace(teksti, @"vana", "uusi");
Console.WriteLine(u