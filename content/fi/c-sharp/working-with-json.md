---
title:                "C#: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi: JSON-tiedostojen käsittelyn tärkeys

JSON (JavaScript Object Notation) on yleisesti käytetty tiedostomuoto, joka on helppolukuinen sekä ihmisten että tietokoneiden kannalta. Se on myös helppo käsitellä ja tallentaa tiedostoja tietokannassa tai lähettää niitä internetin kautta. Tästä syystä on tärkeää osata työskennellä JSON-tiedostojen kanssa, ja tässä blogikirjoituksessa käymme läpi, miten se tehdään C# -ohjelmointikielellä.

## Miten: JSON-tiedostojen käsittely C# -koodilla

C# tarjoaa useita tapoja käsitellä JSON-tiedostoja. Yksi tapa on käyttää Newtonsoft.Json-pakettia, joka on suosittu kirjasto JSON-tiedostojen käsittelyyn. Esimerkiksi seuraava koodi lukee JSON-tiedoston ja tulostaa sen sisällön konsoliin:

```C#
using Newtonsoft.Json;
using System.IO;

// lue tiedosto ja tallenna se muuttujaan
string json = File.ReadAllText("tietokoneet.json");

// deserialisoi JSON-muotoiseksi listaksi 
var tietokoneet = JsonConvert.DeserializeObject<List<Tietokone>>(json);

// tulosta tietokoneiden tiedot konsoliin
foreach (Tietokone tietokone in tietokoneet)
{
    Console.WriteLine("Malli: " + tietokone.Malli);
    Console.WriteLine("Valmistaja: " + tietokone.Valmistaja);
    Console.WriteLine("Hinta: " + tietokone.Hinta);
}
```

Tässä esimerkkikoodissa käytämme Newtonsoft.Json-pakettia muuntamaan JSON-tiedoston listaksi C#-olioita. Tästä seuraa, että pystymme käsittelemään tiedoston sisältöä helposti C#-koodilla ja esimerkiksi tulostamaan sen tai tallentamaan tietokantaan.

## Deep Dive: JSON-tiedostojen tyypit ja niiden käsittely

JSON-tiedostoissa on useita erilaisia tietotyyppejä, kuten merkkijonoja, numeroita, totuusarvoja ja taulukoita. Kun haluat käsitellä näitä tietotyyppejä C#-koodilla, tarvitset metodeita ja luokkia, jotka osaavat muuttaa ne oikeiksi C#-tyypeiksi. Tämä onnistuu esimerkiksi Newtonsoft.Json paketin avulla, joka tarjoaa erilaisia JSON-muunnosmetodeja.

Voit myös itse luoda JSON-muotoisia tiedostoja C#-ohjelmassa ja tallentaa ne esimerkiksi tietokantaan tai lähettää ne internetin kautta. Tämä on hyödyllistä esimerkiksi silloin, kun haluat lähettää käyttäjän syöttämiä tietoja palvelimelle tai tallentaa sovelluksen asetuksia.

## Katso myös

- [Newtonsoft.Json-dokumentaatio] (https://www.newtonsoft.com/json/help/html/Introduction.htm)
- [Microsoftin JSON-tuki C#-kielilä] (https://msdn.microsoft.com/en-us/library/windows/desktop/dn313102(v=vs.85).aspx)
- [JSONW.NET-dokumentaatio] (https://jsonw.codeplex.com/documentation)