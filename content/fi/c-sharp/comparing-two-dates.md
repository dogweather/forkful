---
title:                "C#: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä on tarpeen vertailla kahta eri päivämäärää. Tämä voi olla hyödyllistä esimerkiksi tapahtumien järjestämisen, aikarajojen asettamisen tai tietokantakyselyjen tekemisen kannalta. On tärkeää tietää, miten verrataan kahta päivämäärää ja mitkä ovat mahdollisia skenaarioita, jotka voivat johtaa virheisiin.

## Kuinka

Vertailemalla kahta päivämäärää C# -ohjelmassasi on suoritettava muutamia vaiheita. Ensinnäkin sinun on määritettävä kaikki tarvittavat muuttujat, kuten DateTime-objektit. Sitten voit käyttää DateTime-ominaisuutta "Compare" verratakseksesi kahta päivämäärää. Tämä palauttaa kokonaisluvun, joka osoittaa, onko ensimmäinen päivämäärä ennen, jälkeen vai samana päivänä kuin toinen päivämäärä. Voit myös käyttää "Equals" -metodia tarkistaaksesi, ovatko päivämäärät täsmälleen samoja. Alla olevassa koodiesimerkissä näytämme, miten tämä voidaan toteuttaa.

```C#
DateTime firstDate = new DateTime(2021, 4, 1);
DateTime secondDate = new DateTime(2021, 4, 5);
int result = firstDate.Compare(secondDate);
bool isEqual = firstDate.Equals(secondDate);

Console.WriteLine("Tulos: " + result); // Tuloste: -1
Console.WriteLine("Samoja päivämääriä? " + isEqual); // Tuloste: False
```

On myös tärkeää huomata, että voit käyttää myös muita vertailuoperaattoreita, kuten ">", "<" ja "==", kun verrataan päivämääriä. Tässä on esimerkki siitä, miten voit tehdä tämän:

```C#
DateTime firstDate = new DateTime(2021, 4, 1);
DateTime secondDate = new DateTime(2021, 4, 5);

Console.WriteLine(firstDate > secondDate); // Tuloste: False
Console.WriteLine(firstDate == secondDate); // Tuloste: False
```

## Syvenny aiheeseen

On tärkeää ymmärtää, että päivämäärät ja aika ovat monimutkaisia aiheita ohjelmoinnissa. Jokaisessa kielessä on omat erityispiirteensä ja sudenkuoppansa. C# -kielen osalta sinun tulisi aina huolehtia siitä, että käytät oikeita ajankohtia, käsittelet aikavyöhykkeitä oikein ja otat huomioon päivämäärien välisten erojen vaikutuksen.

On myös tärkeää huomata, että C# tarjoaa myös erilaisia sisäänrakennettuja työkaluja, kuten DateTime-luokan ja TimeSpan-rakenteen, joiden avulla voit helpottaa päivämäärien käsittelyä ja laskemista. Tutustu C# -dokumentaatioon saadaksesi lisätietoja näistä työkaluista ja kuinka voit käyttää niitä vertailemaan päivämääriä.

## Katso myös

- [C# DateTime -dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# TimeSpan -dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-5.0)
- [Stack Overflow - How to compare two dates](https://stackoverflow.com/questions/40117957/how-to-compare-two-dates-in-c-sharp)