---
title:                "Kahden päivämäärän vertailu"
html_title:           "C#: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärien vertaaminen on yksinkertaisesti kahden eri päivämäärän vertailemista keskenään. Ohjelmoijat tekevät tätä yleensä saadakseen selville, kumpi päivämäärä on aikaisempi tai myöhempi, tai halutakseen tehdä tiettyjä toimintoja sen perusteella.

## Miten:
Päivämäärien vertaaminen C#:lla on helppoa. Voit käyttää `DateTime.Compare` -metodia, joka vertailee kahta päivämäärää ja palauttaa arvon suhteessa toisiinsa. Tässä on yksinkertainen esimerkki:

```C#
DateTime date1 = new DateTime(2020, 04, 26);
DateTime date2 = new DateTime(2021, 10, 15);

int result = DateTime.Compare(date1, date2);

if (result < 0)
{
    Console.WriteLine("Ensimmäinen päivämäärä on aikaisempi.");
}
else if (result > 0)
{
    Console.WriteLine("Toinen päivämäärä on aikaisempi.");
}
else
{
    Console.WriteLine("Päivämäärät ovat samat.");
}

// Output:
// Ensimmäinen päivämäärä on aikaisempi.
```

## Syvemmälle:
Päivämäärien vertaaminen on tärkeä osa ohjelmointia, erityisesti aikajärjestelmiin liittyvissä sovelluksissa. Tämän lisäksi on olemassa muita tapoja vertailla päivämääriä, kuten käyttämällä `DateTime.CompareTo` -metodia tai käsin kirjoittamalla vertailulogiikka. Kaikissa näissä tapauksissa kannattaa olla tarkkana, koska esimerkiksi vuoden 2020 helmikuu 29. päivää ei ole olemassa, joten päivämäärien vertaamiseen tulee kiinnittää huomiota.

## Katso myös:
Voit oppia lisää päivämäärien vertaamisesta C#:lla täältä: [C# DateTime.Compare() Method](https://www.c-sharpcorner.com/UploadFile/puranindia/datetime-compare-in-C-Sharp/).