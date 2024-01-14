---
title:    "C#: Merkkijonojen yhdistäminen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi: Yhteenveto merkkijonojen yhdistämisen tärkeydestä 
Merkkijonojen yhdistäminen on oleellinen osa ohjelmoinnin perusteita. Kun ohjelma toimii, se olisi kyettävä tulostamaan tietoa käyttäjälle selkeässä muodossa. Tätä varten sinun täytyy pystyä yhdistämään erilaisia merkkijonoja ja muuttujia saadaksesi haluamasi lopputuloksen. Näin voit luoda selkeämmän ja helpommin luettavan koodin.

## Miten tehdä: Esimerkkejä koodista ja tulosteista "```C#...```" koodinmuotoimuksessa
Merkkijonojen yhdistäminen C#:ssa tapahtuu käyttämällä "+" -merkkiä ja kahden merkkijonon väliin asetettuja muuttujia. Esimerkiksi jos haluat tulostaa tervehdys viestin jonka käyttäjä voi määrittää, voit käyttää seuraavaa koodia:

```C#
string nimi = "Milla";
Console.WriteLine("Hei " + nimi + ", tervetuloa ohjelmaan!");
```
Tuloste: Hei Milla, tervetuloa ohjelmaan!

Voit myös käyttää .Format-metodia yhdistääksesi merkkijonoja C#:ssa. Se näyttää tältä:

```C#
string kaupunki = "Helsinki";
int vuosi = 2021;
Console.WriteLine(string.Format("Tervetuloa kaupunkiin {0} ja tervetuloa vuoteen {1}!", kaupunki, vuosi));
```
Tuloste: Tervetuloa kaupunkiin Helsinki ja tervetuloa vuoteen 2021!

## Syväkatsaus: Lisätietoja merkkijonojen yhdistämisestä
C#:ssa on tärkeää tietää, miten eri tietotyypit toimivat yhdessä. Kun yhdistät esimerkiksi muuttujan ja merkkijonon, muuttujan arvo muutetaan automaattisesti merkkijonoksi. Voit myös käyttää string.Intepret-metodia muuntaaksesi muuttujan arvon merkkijonoksi.

Toinen tärkeä osa merkkijonojen yhdistämistä on varmistaa, että muuttujien ja merkkijonojen oikea muotoilu on otettu huomioon. Tämä voi sisältää esimerkiksi välilyöntien lisäämisen tai käyttäjän syötteen validoinnin.

Oikean muotoilun lisäksi on myös tärkeää puhdistaa merkkijonoja ennen niiden yhdistämistä. Voit esimerkiksi käyttää string.Trim-metodia poistamaan ylimääräiset välilyönnit tai string.ToLower-metodia muuttamaan tekstin pieniksi kirjaimiksi.

## Katso myös
- Microsoftin ohjeet merkkijonojen yhdistämisestä C#:ssa https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=netframework-4.8
- C# -tyyppien muunnos https://www.tutorialspoint.com/csharp/csharp_type_casting.htm
- Merkkijonojen muotoilu C#:ssa https://www.tutorialsteacher.com/csharp/csharp-string-format