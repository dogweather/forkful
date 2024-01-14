---
title:                "C#: Päivämäärän muuntaminen merkkijonoksi"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa C# -sovelluksissa on tarve muuttaa päivämäärä muotoon, joka voidaan esittää merkkijonona. Tämä voi olla tarpeellista esimerkiksi, kun päivämäärällä halutaan näyttää käyttäjälle tietoa joltakin tietyltä ajalta.

## Näin teet sen

Date-luokka tarjoaa erilaisia ​​menetelmiä päivämäärän muuttamiseen merkkijonoksi. Tässä esimerkissä käytämme ToShortDateString()-menetelmää, joka muuttaa päivämäärän lyhyeksi merkkijonoksi. Koodilohko näyttää, kuinka tämä tehdään:

```C#
// Luodaan uusi DateTime-olio  
DateTime date = new DateTime(2020, 10, 15);

// Muutetaan päivämäärä merkkijonoksi
string dateString = date.ToShortDateString();

// Tulostetaan tulos
Console.WriteLine(dateString);

```

Tulostus tästä koodilohkosta olisi: 15.10.2020. Voit myös muuttaa päivämäärän erilaiseen muotoon, kuten esimerkiksi ToLongDateString()-menetelmällä, joka tulostaisi esimerkiksi "15. lokakuuta 2020".

## Syvempää tietoa

C# tukee erilaisia ​​kielellisiä ominaisuuksia päivämäärämuunnoksia varten. Esimerkiksi voit käyttää muotoilumerkkijonoja määrittämään tarkemman muotoilun haluamallesi päivämäärän esitystavalle. Voit myös käyttää CultureInfo-luokkaa, joka tarjoaa tietoja eri kulttuureiden päivämäärän kirjoitus- ja lukutavasta. Täällä on lisää esimerkkejä eri päivämäärän muunnoksiin:

- [C# DateTime -päivämäärän muunnos](https://www.w3schools.com/cs/cs_date_tostring.asp)
- [Muotoilumerkkijonot C# päivämäärämuunnoksille](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo-luokan käyttö päivämäärämuunnoksissa](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)

## Katso myös

- [Päivämäärän muuntaminen SQL:stä stringiksi C#](https://www.c-sharpcorner.com/UploadFile/mahesh/converting-datetime-to-string-and-vice-versa-in-c-Sharp/)
- [C# -päivämääräluokat ja niiden käyttö](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [Päivämäärän validointi C# -sovelluksissa](https://www.c-sharpcorner.com/article/validate-datetime-in-c-sharp/)