---
title:    "C#: Päivämäärien vertailu"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahden päivämäärän välillä?

Päivämäärien vertailu on yleinen ohjelmoinnin tehtävä, jota käytetään esimerkiksi tapahtumien aikajärjestyksen selvittämiseen tai päivämäärien erotuksen laskemiseen. Tämä artikkeli opastaa kuinka voit vertailla kahta päivämäärää C#-ohjelmointikielellä.

## Kuinka tehdä se?

Päivämäärien vertailu C#-ohjelmointikielessä on helppoa. Alla olevassa esimerkissä käytämme `DateTime`-luokkaa ja sen `Compare`-metodia vertaillaksemme kahta päivämäärää.

```C#
DateTime date1 = new DateTime(2021, 10, 15);
DateTime date2 = new DateTime(2021, 11, 20);

int result = DateTime.Compare(date1, date2);

if(result < 0)
{
    Console.WriteLine(date1 + " on aiempi päivämäärä kuin " + date2);
}
else if(result > 0)
{
    Console.WriteLine(date2 + " on aiempi päivämäärä kuin " + date1);
}
else
{
    Console.WriteLine(date1 + " ja " + date2 + " ovat samat päivämäärät");
}
```

Tämä koodi vertailee kahta päivämäärää ja tulostaa konsoliin niiden välistä suhdetta. Tuloksena saamme:

```
15.10.2021 on aiempi päivämäärä kuin 20.11.2021
```

Voit myös käyttää `DateTime`-luokan muita metodeja, kuten `Equals` ja `CompareTo`, päivämäärien vertailuun. On tärkeää muistaa, että vertailu perustuu päivämäärien aikajärjestykseen ja että päivämäärät kirjoitetaan oikeassa muodossa, esimerkiksi `DateTime(2021, 1, 15)`.

## Syventävä tieto

Päivämäärien vertailu voi tuntua yksinkertaiselta, mutta syvemmälle kaivettaessa huomaamme, että se perustuu `DateTime`-luokan taustalla olevaan aikaleimasimeen, joka laskee päiviä 01.01.0001:n jälkeen. On myös tärkeää ottaa huomioon aikavyöhykkeet ja päivämäärien muotoilut, kun vertailee päivämääriä.

Voit lukea lisää `DateTime`-luokasta ja sen käyttömahdollisuuksista Microsoftin virallisilta verkkosivuilta.

## Katso myös

- [C# DateTime Class](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [DateTime.Compare Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-5.0)
- [C# Tietotyypit ja muunnokset](https://www.tktutor.org/k/csharp/csharp-tyypit.htm)