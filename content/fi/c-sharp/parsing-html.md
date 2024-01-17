---
title:                "Html:n jäsentäminen"
html_title:           "C#: Html:n jäsentäminen"
simple_title:         "Html:n jäsentäminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTML:n jäsentäminen ("parsing") tarkoittaa HTML-koodin lukemista ja sen sisältämien elementtien tunnistamista. Tämä on tärkeää, koska ohjelmoijat usein haluavat luoda ohjelmia, jotka pystyvät käsittelemään ja muokkaamaan HTML-tiedostoja automaattisesti.

## Miten:
```C#
using System;
using HtmlAgilityPack;

var url = "www.example.com";
var web = new HtmlWeb();
var doc = web.Load(url);
var nodes = doc.DocumentNode.SelectNodes("//a[@href]");

foreach(var node in nodes)
{
    Console.WriteLine(node.GetAttributeValue("href", ""));
}
```
Tässä esimerkissä käytetään HtmlAgilityPack-kirjastoa, joka tarjoaa valmiita työkaluja HTML:n jäsentämiseen. Ensimmäisillä riveillä asetetaan muuttujat url-osoitteelle ja HtmlWeb-olioon. Seuraavaksi ladataan HTML-tiedosto ja etsitään siitä kaikki a-elementit sen attribuutilla "href". Lopuksi tulostetaan löytyneiden elementtien "href"-attribuutit.

## Pureutuminen:
HTML:n jäsentämisen tarve on alkanut kasvaa Internetin kehityksen myötä. Aikaisemmin sivustot olivat staattisia ja sisälsivät vähemmän elementtejä, joten niiden käsittely ei ollut niin tärkeää. Nykyään sivustot ovat monimutkaisia ja sisältävät paljon erilaisia elementtejä, joten tarve jäsentämiseen on suurempi. Vaihtoehtoisia tapoja jäsentämiseen ovat esimerkiksi regex eli säännölliset lausekkeet ja manuaalinen käsittely, mutta ne ovat usein huomattavasti hankalampia ja epäluotettavampia kuin erilaiset kirjastot. HTML:n jäsentämisessä käytetään yleensä erilaisia XPath-kyselyitä löytämään haluttuja elementtejä. Tekniikan käytöstä löytyy paljon oppaita ja esimerkkejä, joten sen omaksuminen ei ole vaikeaa.

## Katso myös:
- [HtmlAgilityPack-dokumentaatio](https://html-agility-pack.net/documentation)
- [XPath-tutoriaali](https://www.w3schools.com/xml/xpath_intro.asp)
- [Esimerkki HTML:n jäsentämisestä Pythonilla](https://realpython.com/parsing-html-web-page/)