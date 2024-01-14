---
title:                "C#: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Miksi lähettää HTTP-pyyntö?

HTTP-pyyntöjen lähettäminen on tärkeä osa web-sovellusten kehittämistä. Se mahdollistaa kommunikaation käyttäjän ja palvelimen välillä ja siten mahdollistaa dynaamisten ja responsiivisten sovellusten luomisen. Esimerkiksi lomakkeiden lähettäminen, resurssien hakeminen palvelimelta ja tiedon päivittäminen ovat kaikki esimerkkejä HTTP-pyynnöistä.

# Kuinka lähettää HTTP-pyyntö?

Voit lähettää HTTP-pyynnön C# -ohjelmalla käyttämällä HttpClient-luokkaa. Alla olevassa koodiesimerkissä näytämme, kuinka lähettää GET-pyyntö Google-hakukoneelle ja tulostaa vastauksen sisältö konsoliin.

```C#

using System;
using System.Net.Http;

class Program
{
    static async Task Main(string[] args)
    {
        // Luodaan uusi HttpClient-olio
        HttpClient client = new HttpClient();

        // Lähetetään GET-pyyntö ja tallennetaan vastaus
        HttpResponseMessage response = await client.GetAsync("https://www.google.com/");

        // Tulostetaan vastauksen sisältö
        string responseContent = await response.Content.ReadAsStringAsync();
        Console.WriteLine(responseContent);
    }
}

```

Koodin ajamisen jälkeen consoleen tulisi tulostua koko HTML-sisältö Google-hakusivulta.

# Syvällinen sukellus HTTP-pyyntöihin

HTTP-pyynnöt koostuvat useista eri osista, kuten URL-osoitteesta, HTTP-metodista ja pyynnön rungosta. Voit mukauttaa näitä osia lähettämiisi pyyntöihin jos tarpeen.

Lisäksi, jos haluat työskennellä requestien kanssa asynkronisesti, voit käyttää Moderni Web-palveluiden kevytrakennetta (ASP.net Web API) tai RESTful-palveluita.

Viimeiseksi, muista aina suojata tärkeät tiedot, kuten käyttäjän salasanat, lähettäessäsi HTTP-pyyntöjä. Voit käyttää HTTPS-protokollaa tietoturvan varmistamiseksi.

# Katso myös

- [HTTP-pyynnön lähettäminen käyttäen C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=netframework-4.8)
- [Tietoa HTTP-protokollasta](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Web API ja RESTful-palvelut](https://www.c-sharpcorner.com/UploadFile/dhananjaycoder/introduction-to-web-api-and-restful-services/)