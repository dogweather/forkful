---
title:                "C#: Kahden päivämäärän vertaaminen"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahden päivämäärän välillä?

Päivämäärien vertaaminen voi olla hyödyllistä esimerkiksi ohjelmassa, jossa on tarvetta tarkistaa, onko annettu päivämäärä tulevaisuudessa vai menneisyydessä. Tämä voi olla tarpeellista esimerkiksi järjestelmän lokien tarkistamisessa tai tulevien tehtävien aikatauluttamisessa.

## Miten vertailla kahden päivämäärän välillä?

Käytämme C# -ohjelmointikielen Date-tyyppiä vertaillaksemme kahta päivämäärää. Tämän tyyppinen vertailu palauttaa boolean-arvon, joka ilmaisee, onko ensimmäinen päivämäärä ennen vai jälkeen toista päivämäärää. Alla on esimerkki koodista, jossa verrataan kahta päivämäärää sekä tulostetaan vertailun tulos konsoliin.

```C#
using System;

class Program
{
    static void Main()
    {
        // Luodaan kaksi Date-tyyppistä muuttujaa
        DateTime date1 = new DateTime(2021, 12, 24);
        DateTime date2 = new DateTime(2022, 1, 1);

        // Verrataan päivämääriä ja tulostetaan tulos konsoliin
        if (date1 < date2)
        {
            Console.WriteLine("Ensimmäinen päivämäärä on ennen toista päivämäärää.");
        }
        else if (date2 < date1)
        {
            Console.WriteLine("Toinen päivämäärä on ennen ensimmäistä päivämäärää.");
        }
        else
        {
            Console.WriteLine("Päivämäärät ovat samat.");
        }
    }
}
```

Tulostus:

```
Ensimmäinen päivämäärä on ennen toista päivämäärää.
```

Voimme myös käyttää muita vertailuoperaattoreita, kuten `<=` ja `>=`, mikä antaa meille mahdollisuuden tarkastella myös päivämäärien samanaikaisuutta.

## Syvällisempi sukellus

Päivämäärät saattavat tuntua yksinkertaisilta ja helppokäyttöisiltä tietotyypeiltä, mutta niiden taustalta löytyy monimutkaisempaa logiikkaa. Esimerkiksi vuosien, kuukausien ja päivien eri pituudet vaikuttavat vertailuun ja ovat tärkeitä ottaa huomioon.

Lisäksi C# tarjoaa monia muita valmiita työkaluja päivämäärien käsittelyyn, kuten `DateTime.TryParse()` ja `DateTime.ParseExact()`. Nämä voivat olla hyödyllisiä, jos ohjelmaan syötetään päivämäärä käyttäjän antamana merkkijonona.

## Katso myös

- [C# Date-tietotyyppi (Microsoftin dokumentaatio)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Päivämäärien vertailu JavaScriptissä (blogikirjoitus)](https://www.w3schools.com/jsref/jsref_obj_date.asp)