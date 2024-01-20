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

## Mikä & Miksi?

Web-sivun lataaminen on prosessi, jossa tietokone hakee ja tallentaa verkkosivun tiedot. Ohjelmoijat tekevät sen hakiessaan tai kerätessään tietoa verkosta.

## Miten:

Katsotaanpa esimerkkiä, jossa käytetään `HttpClient` luokkaa:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        var url = "http://example.com";
        var responseString = await client.GetStringAsync(url);
        Console.WriteLine(responseString);
    }
}
```

Ohjelma hakee sisällön osoitteesta `http://example.com` ja tulostaa sen konsoliin.


## Syvempää keskustelua:

- Historiallinen konteksti: Alun perin verkkosivujen lataaminen tehtiin käyttäen `WebRequest` / `WebResponse` -luokkia. Myöhemmin `HttpClient` esiteltiin yksinkertaistamaan ja parantamaan tätä prosessia.
  
- Vaihtoehdot: Muita vaihtoehtoja ovat kolmansien osapuolien kirjastot, kuten `RestSharp` tai `Flurl.Http`, jotka tarjoavat lisäominaisuuksia ja yksinkertaisemman käyttöliittymän.
  
- Toteutuksen yksityiskohdat: `HttpClient.GetStringAsync` -metodin takana tapahtuu paljon. Se luo HTTP GET -pyynnön määriteltyyn URL-osoitteeseen, odottaa vastausta, lataa vastauksen sisällön ja palauttaa sen merkkijonona.

## Katso myös:

- Microsoftin dokumentaatio HttpClient:ille: https://docs.microsoft.com/fi-fi/dotnet/api/system.net.http.httpclient
- Tutkittava blogikirjoitus HttpClientin käytöstä: https://johnthiriet.com/efficient-api-calls/
- Lisää tietoa web-satunnaisten käytöstä c#:ssa: https://www.pluralsight.com/courses/csharp-httpfundamentals