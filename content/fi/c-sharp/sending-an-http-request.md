---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "C#: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

HTTP-pyyntöjen lähettäminen on tärkeä osa nykyaikaista ohjelmointia. Se on tapa, jolla ohjelmoijat voivat kommunikoida verkkopalveluiden kanssa ja hakea tietoa. Ohjelmoijat lähettävät HTTP-pyyntöjä esimerkiksi silloin, kun he haluavat saada tietoa verkosta tai päivittää tietokantoja.

## Näin teet sen:

```C#
// Lähetetään HTTP GET -pyyntö osoitteeseen www.example.com
WebRequest pyynto = WebRequest.Create("http://www.example.com");
WebResponse vastaus = pyynto.GetResponse();

// Luetaan vastauksen sisältö
Stream dataVirta = vastaus.GetResponseStream();
StreamReader lukija = new StreamReader(dataVirta);
string sisalto = lukija.ReadToEnd();

// Tulostetaan vastauksen sisältö konsoliin
Console.WriteLine(sisalto);

// Suljetaan vastaus
vastaus.Close();
```

Esimerkkilähdön tuloste:

`<!doctype html><html lang="fi"><head><title>Esimerkki</title></head><body><h1>Tervetuloa!</h1></body></html>`

## Syväsukellus:

HTTP-pyyntöjen lähettäminen sai alkunsa jo 1990-luvulla, kun ensimmäiset verkkopalvelut alkoivat yleistyä. Siitä lähtien käytössä on ollut myös muita tapoja lähettää pyyntöjä, kuten esimerkiksi SOAP ja REST. C#:ssa HTTP-pyyntöjen lähettämiseen voi käyttää esimerkiksi WebRequest- tai HttpClient-olioita. On myös mahdollista säätää pyyntöön erilaisia parametreja, kuten käyttäjätunnusta ja salasanaa.

## Katso myös:

- [C#:n WebRequest-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.net.webrequest?view=net-5.0)
- [HttpClient-luokan käyttöohjeet](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [HTTP-pyyntöjen lähettäminen: GET ja POST](https://www.w3schools.com/tags/ref_httpmethods.asp)