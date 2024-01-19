---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Päivämäärän hankkiminen C#-ohjelmointikielessä

## Mikä ja miksi?
Päivämäärän hankkiminen tarkoittaa järjestelmän hetkellisen päivämäärän ja kellonajan noutamista. Ohjelmoijat tekevät tämän lukuisista syistä, kuten tiedostojen ajastamiseen, tapahtumien ajoittamiseen tai aikaleimojen luomiseen.

## Näin se tehdään:
Päivämäärän hankkimiseksi C#-kielessä voimme hyödyntää DateTime-oliota.

```C#
using System;

public class Program
{
    public static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine("Current date: " + currentDate);
    }
}
```
Tämän koodin suorittaminen tulostaa hetkellisen päivämäärän ja kellonajan, esimerkiksi `Current date: 4.5.2022 16:30:55`

## Syvä sukellus
Historiallisesti päivämäärän ja ajan hankkiminen ei ole aina ollut yhtä helppoa kuin tänä päivänä. Vanhemmissa kielissä, kuten C:ssä, tämä vaati usein monimutkaisempia menetelmiä.

Vaihtoehtoisesti C#-kielessä on myös mahdollista käyttää DateTime.UtcNow –metodia, jolla saadaan UTC-muotoinen hetkellinen päivämäärä ja aika. Tämä on hyödyllistä, kun halutaan välttää aikavyöhykkeistä johtuvat erot.

```C#
using System;

public class Program
{
    public static void Main()
    {
        DateTime currentDate = DateTime.UtcNow;
        Console.WriteLine("Current UTC date: " + currentDate);
    }
}
```

C# implementoi päivämäärän ja ajan hankinnan DateTime-olion avulla, joka on osa System-nimiavaruutta. DateTime sisältää monia metodeja, joilla voimme manipuloida ja kysellä päivämäärää ja aikaa.

## Katso myös
- Microsoftin ohjeet DateTime-olion käyttämiseen: [Tutustu DateTime-dokumentaatioon](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime)
- Stack Overflow -keskustelu eri tavasta hankkia aikaa: [Tutustu keskusteluun](https://stackoverflow.com/questions/215497/in-c-how-do-i-get-the-current-date)