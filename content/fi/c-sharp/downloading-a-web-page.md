---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Haluan selata suosikkisivustojani offline-tilassa, joten tarvitsen tapoja ladata web-sivuja ja tallentaa ne omalle laitteelleni.

## Näin teet sen

Tärkein työkalu web-sivujen lataamiseen on C#-ohjelmointikieli, joka tarjoaa kätevät toiminnot HTTP-pyyntöjen lähettämiseen ja vastaanottamiseen. Alla on esimerkki koodista, joka lataa sivun ja tallentaa sen tekstitiedostoksi:

```C#
using System;
using System.Net;

string url = "https://www.example.com";
string fileName = "example.txt";

using (var client = new WebClient())
{
  string htmlCode = client.DownloadString(url);

  System.IO.File.WriteAllText(fileName, htmlCode);
  Console.WriteLine("Sivu ladattu ja tallennettu tiedostoon " + fileName);
}
```

Tämä pieni pala koodia käyttää ``WebClient``-luokkaa tekemään GET-pyynnön valitulle URL-osoitteelle. Paluuarvona saadaan HTML-koodi, joka tallennetaan tekstitiedostoksi ``example.txt`` käyttäen ``File.WriteAllText()``-metodia. Lopuksi tulostetaan viesti, joka kertoo onnistuneesta latauksesta.

Voit myös käyttää muita C#:n tarjoamia HTTP-kirjastoja, kuten ``HttpClient`` tai ``HttpWebRequest``, joilla on omat ominaisuutensa ja toiminnallisuutensa.

## Syväsukellus

Web-sivujen lataaminen ei ole aina yksinkertaista, koska ne voivat sisältää erilaisia elementtejä, kuten HTML-taulukoita, kuvia, JavaScript-koodia jne. On tärkeää tutkia ja ymmärtää sivun rakennetta ennen sen lataamista.

Lisäksi, jos sivu vaatii kirjautumisen tai muuta tunnistautumista, on ensin luotava automatisoitu kirjautumisprosessi ennen lataamista. Tässä tapauksessa esimerkiksi ``WebClient``-luokan sijaan kannattaa käyttää ``HttpClient``-luokkaa, jolla voi lähettää pyynnön ja vastaanottaa vastauksen, johon sisältyy tarvittavat evästeet käyttäjän tunnistamiseksi.

## Katso myös

- [WebClient-luokka (Virallinen dokumentaatio)](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [HttpClient-luokka (Virallinen dokumentaatio)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [HttpWebRequest-luokka (Virallinen dokumentaatio)](https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest?view=net-5.0)