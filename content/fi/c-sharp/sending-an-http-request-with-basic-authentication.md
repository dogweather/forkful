---
title:                "C#: Lähettämällä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettämällä http-pyyntö perusautentikoinnilla"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen perustason todennuksella on tärkeä taito jokaiselle C# -kehittäjälle, sillä se mahdollistaa turvallisen ja suojatun tiedonsiirron verkon yli. Tämä artikkeli esittelee, miten lähettää HTTP-pyyntö perustason todennuksella ja tarjoaa syvällisempää tietoa aiheesta.

## Miten tehdä se

C# -kielen avulla HTTP-pyyntöjen lähettäminen perustason todennuksella on helppoa. Se onnistuu seuraavilla askelilla:

1. Luo HttpClient -olio ja aseta oletusarvoinen BaseAddress -ominaisuus URL-osoitteelle, johon haluat lähettää pyynnön.
2. Luo AuthenticationHeaderValue -olio ja aseta se HttpClient -olion DefaultRequestHeaders -ominaisuuteen.
3. Luo HttpRequestMessage -olio ja aseta sen Method -ominaisuuteen haluamasi HTTP-metodi.
4. Lisää tarvittavat otsikot ja/tai sisältö pyyntöön.
5. Lähetä pyyntö HttpClient -olion SendAsync -metodin avulla.
6. Käsittele vastaus tarvittaessa.

Seuraava esimerkki näyttää, miten lähettää GET-pyyntö perustason todennuksella:

```C#
using System;
using System.Net.Http;
using System.Net;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        // Luo HttpClient -olio ja aseta oletusarvoinen BaseAddress
        var client = new HttpClient();
        client.BaseAddress = new Uri("https://www.example.com");

        // Luo AuthenticationHeaderValue -olio ja aseta se DefaultRequestHeaders -ominaisuuteen
        var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
        client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", credentials);

        // Luo HttpRequestMessage -olio ja aseta sen Method -ominaisuuteen haluttu metodi
        var request = new HttpRequestMessage(HttpMethod.Get, "/api/users");

        // Lähetä pyyntö ja odota vastausta
        var response = await client.SendAsync(request);

        // Tulosta vastaus pyytämällä sisältö string-muodossa
        Console.WriteLine(await response.Content.ReadAsStringAsync());
    }
}
```

Esimerkistä näemme, että HTTP-pyynnön lähettäminen perustason todennuksella vaatii vain muutaman rivin koodia ja asetusten määrittämisen.

## Syvällisempi tarkastelu

Perustason todennuksen käyttäminen HTTP-pyyntöjen lähettämisessä tarjoaa helpon tavan varmistaa, että vain oikeilla käyttäjillä on pääsy tiettyyn resurssiin verkon yli. Todennus tapahtuu lähettämällä käyttäjän tunnistetiedot salattuna Base64-muodossa pyynnön otsikossa. Vastaanottava palvelin tarkistaa tunnistetiedot ja sallii pääsyn resurssiin, jos ne ovat oikein.

On tärkeää muistaa, että perustason todennus ei tarjoa vahvaa turvasuojausta, sillä tunnistetiedot lähetetään avoimesti ja ne on helppo purkaa. Siksi sitä suositellaan käytettäväksi vain joillekin julkisille verkkosivustoille, joissa tiedon ei tarvitse olla hakkerien ulottuvilla.

## Katso myös

- [HTTP-pyynnön lähettäminen C# -kielellä (Microsoftin dokumentaatio)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Verkkotunnusten todennus (Wikipedian artikkeli suomeksi)](https://fi.wikipedia.org/wiki/Verkkotunnusten_todennus)