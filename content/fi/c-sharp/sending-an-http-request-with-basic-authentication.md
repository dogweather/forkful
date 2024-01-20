---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lähettäessämme HTTP-pyynnön perustunnistuksen kanssa, lähetämme Internet-palvelimelle pyynnön, joka sisältää käyttäjänimen ja salasanan suoritettaessa erilaisia ​​tehtäviä. Ohjelmoijat tekevät tämän tietojen turvallisen jakamisen ja suojattujen resurssien saamiseksi.

## Kuinka:

Lähetä HTTP-pyyntö perustunnistuksen kanssa C#-koodilla:

```C#
using System;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

namespace BasicAuthentication
{
    class Program
    {
        static async Task Main()
        {
            var httpClient = new HttpClient();
            var byteArray = Encoding.ASCII.GetBytes("username:password");
            httpClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

            HttpResponseMessage response = await httpClient.GetAsync("https://example.com");

            Console.WriteLine(response.StatusCode);
        }
    }
}
```
Koodin ajo tulostaa HTTP-koodin, joka osoittaa, menikö pyyntö läpi vai ei.

## Syvällisemmin:

### Historiallinen Konteksti:
HTTP-perustunnistus on yksi vanhimmista tavaroista, jotka ovat osa HTTP-protokollaa ja jota kehittäjät käyttävät edelleen tietoturvaan.

### Vaihtoehdot:
Vaikka perustunnistusta käytetään yleisesti, se saattaa olla haavoittuvainen "man-in-the-middle" hyökkäyksille. Siksi vaihtoehtoisia menetelmiä, kuten OAuth ja token-perusteinen tunnistautuminen, kannattaa harkita.

### Toteutuksen yksityiskohdat:
Käyttäjänimen ja salasanan lähettäminen suoraan verkon yli ei ole turvallista. Sen sijaan ne on koodattava Base64-koodauksessa, kuten esimerkissämme näytettiin.

## Katso Lisäksi:

HTTPS-tiedonsiirron turvaaminen: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
HTTP-pyynnön lähettämisen useita esimerkkejä: [Microsoft Documentation](https://docs.microsoft.com/en-fi/azure/architecture/best-practices/api-design)
OAuth- ja token-pohjainen autentikointi: [Introduction to OAuth](https://oauth.net/2/), [Token-based Authentication](https://www.varonis.com/blog/token-based-authentication/)