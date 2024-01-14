---
title:                "C#: Yhdistävät merkkijonot"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit yhdistää merkkijonoja? Yhdistäminen on hyödyllinen tapa muodostaa pidempiä merkkijonoja, kuten esimerkiksi tekstin muodostaminen käyttäjän antamista syötteistä.

## Kuinka
Yhdistäminen tapahtuu yksinkertaisesti käyttämällä "+" operaattoria, joka yhdistää kaksi merkkijonoa toisiinsa. Alla on esimerkki C# koodista, joka yhdistää kaksi merkkijonoa ja tulostaa lopputuloksen konsolille.

```C#
string tervetuloaViesti = "Tervetuloa ";
string nimi = "Matti";
Console.WriteLine(tervetuloaViesti + nimi);
```

Tämän koodin tuloste olisi "Tervetuloa Matti". Voit myös yhdistää useampia merkkijonoja samalla tavalla, lisäämällä "+" operaattorin jokaisen merkkijonon väliin.

Voit myös yhdistää merkkijonoja muuttujien avulla, mikä tekee koodista helpommin muokattavaa ja ylläpidettävää. Alla olevassa esimerkissä käytämme string.Format() metodia, joka yhdistää annetut merkkijonot annetulla muodolla.

```C#
string keittioViesti = string.Format("Keittiössä on {0} ruokaa ja {1} juomaa.", "pasta", "mehu");
Console.WriteLine(keittioViesti);
```

Tämän koodin tuloste olisi "Keittiössä on pasta ruokaa ja mehu juomaa". Huomaa, että string.Format() metodissa annettavat muuttujat numeroidaan alkaen nollasta.

## Syvempi sukellus
Saatamme joskus törmätä tilanteisiin, joissa on tarpeen yhdistää suuri määrä merkkijonoja. Tässä tapauksessa, sen sijaan että käyttäisimme monta "+" operaattoria, voimme käyttää StringBuilder luokkaa, joka on tehokkaampi yhdistämään suuria määriä merkkijonoja.

StringBuilder luokkaan kuuluu Append() metodi, joka yhdistää annetun merkkijonon loppuun ja ToString() metodi, joka muuttaa kaikki yhdistetyt merkkijonot yhdeksi merkkijonoksi. Alla esimerkki käyttäen StringBuilder luokkaa.

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Ensimmäinen lause. ");
sb.Append("Toinen lause. ");
sb.Append("Kolmas lause. ");
Console.WriteLine(sb.ToString());
```

Tämä tulostaisi "Ensimmäinen lause. Toinen lause. Kolmas lause." StringBuilder luokalla on myös muita hyödyllisiä metodeita, kuten Insert() ja Replace(), jotka voivat auttaa tarkemmissa manipulaatioissa merkkijonojen yhdistämisessä.

## Katso myös
- Microsoftin virallinen dokumentaatio C# merkkijonojen yhdistämisestä: https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings
- Tutorialspointin opetusohjelma C# merkkijonojen yhdistämisestä: https://www.tutorialspoint.com/csharp/csharp_string_concat.htm
- C#-Cornerin artikkeli käyttäjien syötteiden yhdistämisestä: https://www.c-sharpcorner.com/article/string-concatenation-in-C-Sharp/