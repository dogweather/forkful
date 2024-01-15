---
title:                "Perusautentikointi http-pyynnön lähettäminen"
html_title:           "C#: Perusautentikointi http-pyynnön lähettäminen"
simple_title:         "Perusautentikointi http-pyynnön lähettäminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyynnön lähettäminen perusautentikoinnilla on tärkeää silloin, kun halutaan suojata tietokantaan tallennettuja arkaluonteisia tietoja tai varmistaa, että vain oikeat käyttäjät pääsevät tiettyihin tietoihin verkkosivustolla.

## Kuinka

Lähetä määritetty HTTP-pyyntö perusautentikoinnilla seuraavalla tavalla:

```C#
var request = (HttpWebRequest)WebRequest.Create("https://example.com/api/endpoint");

//Lisää perusautentikointi otsikkoon
request.Headers.Add("Authorization", "Basic " + Convert.ToBase64String(Encoding.ASCII.GetBytes("käyttäjänimi:salasana")));

var response = (HttpWebResponse)request.GetResponse();
Console.WriteLine($"Response Code: {response.StatusCode} {response.StatusDescription}");
Console.WriteLine("Response Body:");
using (var dataStream = response.GetResponseStream())
{
    StreamReader reader = new StreamReader(dataStream);
    string responseFromServer = reader.ReadToEnd();
    Console.WriteLine(responseFromServer);
}
```

Ohjelmassa lähetetään GET-pyyntö osoitteeseen "https://example.com/api/endpoint" käyttäen perusautentikointia. Käyttäjänimi ja salasana on ensin koodattu Base64-muotoon ja lisätty otsikkoon "Authorization". Vastauskoodi ja -viesti tulostetaan konsolille, ja vastauksen sisältö luetaan ja tulostetaan myös.

## Syväsukellus

Perusautentikoinnissa käytetään HTTP-otsikossa olevaa "Authorization" -kenttää, joka koostuu käyttäjän käyttäjänimestä ja salasanasta, erotettuna kaksoispisteellä. Tämän jälkeen tiedot koodataan Base64-muotoon ja lisätään sanan "Basic" perään. Esimerkiksi käyttäjänimellä "user" ja salasanalla "password" otsikko näyttäisi tältä: "Authorization: Basic dXNlcjpwYXNzd29yZA==". On tärkeää huomata, että Base64-koodaus ei ole todellinen salausmenetelmä, vaan se on helppo purkaa. Siksi perusautentikointi ei ole turvallinen tapa suojata arkaluonteisia tietoja, vaan tätä tarkoitusta varten suositellaan käytettäväksi HTTPS-yhteyttä ja esimerkiksi OAuth-autentikointia.

## Katso myös

- [HttpWebRequest-luokka] (https://docs.microsoft.com/en-us/dotnet/api/system.net.httpwebrequest?view=net-5.0)
- [Using Basic authentication with HttpWebRequest] (https://docs.microsoft.com/en-us/dotnet/api/system.web.httprequest.headers?view=net-5.0#System_Web_HttpRequest_Headers)