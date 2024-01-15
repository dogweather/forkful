---
title:                "Http-pyynnön lähettäminen"
html_title:           "C#: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi lähettää HTTP-pyynnön? Vastaus on yksinkertainen: se on välttämätöntä monissa web-sovelluksissa. Kun käytät selainta käyttäessäsi verkkosivuja tai sovelluksia, lähetät itse asiassa jatkuvasti HTTP-pyynnön palvelimelle saadaksesi tietoa ja sisältöä.

## Miten

Lähettääksesi HTTP-pyynnön C#-koodissa, sinun täytyy käyttää HttpClient-luokkaa. Se on osa System.Net.Http-nimiketilaa ja se tarjoaa kaikki tarvittavat toiminnot. Alla on yksinkertainen koodiesimerkki, joka lähettää GET-pyynnön osoitteeseen "www.example.com":

```C#
using System;
using System.Net.Http;

class Program
{
    static async Task Main()
    {
        using (var client = new HttpClient())
        {
            HttpResponseMessage response = await client.GetAsync("http://www.example.com");
            response.EnsureSuccessStatusCode();

            string responseBody = await response.Content.ReadAsStringAsync();
            Console.WriteLine(responseBody);
        }
    }
}
```

Koodin tulostus on lista kaikista saatavilla olevista tiedoista, jotka palvelin vastaa pyyntöösi.

## Syvempi sukellus

HTTP-pyyntö koostuu viidestä pääkomponentista: metodi, URL, otsakkokentät, sisältö ja versio. Metodi määrittää, mitä haluat tehdä pyynnön kanssa, ja yleisimmät metodit ovat GET, POST, PUT ja DELETE. URL kertoo palvelimelle, mihin resurssiin haluat päästä käsiksi. Otsikkokentät ovat lisätietoja pyynnöstä, kuten käyttäjäagentti, joka kertoo palvelimelle, millä selaimella tai sovelluksella pyyntö tehtiin. Sisältö on pyynnön kehon teksti, esimerkiksi kun lähetät POST-pyynnön, jossa lähettää tietoa palvelimelle. Versio kertoo, minkä version HTTP-protokollaa käytät. Näitä kaikkia tietoja voi muokata ja asettaa C#-koodissa koodiesimerkissä käyttämällä erilaisia HttpClient-luokan metodeja.

Katso lisätietoja ja esimerkkejä [Microsoftin dokumentaatiosta](https://docs.microsoft.com/fi-fi/dotnet/api/system.net.http.httpclient?view=net-5.0) ja [Dotnetperls-sivulta](https://www.dotnetperls.com/httpclient). 

## Katso myös

- [Microsoftin dokumentaatio HTTP-pyyntöjen lähettämisestä](https://docs.microsoft.com/fi-fi/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Dotnetperls-sivusto C#-esimerkeillä HTTP-pyyntöjen lähettämisestä](https://www.dotnetperls.com/httpclient)
- [Blogikirjoitus "Understanding HTTP Requests in Web Development" (englanniksi)](https://www.oreilly.com/library/view/web-development-with/9781787125425/47aebc93-632a-4b08-84eb-935a524099c8.xhtml)