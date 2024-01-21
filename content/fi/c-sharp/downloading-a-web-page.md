---
title:                "Verkkosivun lataaminen"
date:                  2024-01-20T17:43:39.363461-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Web-sivun lataaminen on sen sisällön hakemista internetistä ohjelmallisesti. Ohjelmoijat tekevät tämän tiedon käsittelyyn, analysointiin tai varmuuskopiointiin.

## How to: (Kuinka tehdä:)

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class WebPageDownloader
{
    static async Task Main()
    {
        var url = "http://www.example.com";
        using var client = new HttpClient();

        try
        {
            string content = await client.GetStringAsync(url);
            Console.WriteLine(content);
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine($"Error fetching the page: {e.Message}");
        }
    }
}
```

Sample output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (Syväsukellus):

Web-sivun lataamista ohjelmallisesti on tehty lähes niin kauan kuin web on ollut olemassa. Alkuaikoina käytettiin yksinkertaisia skriptejä, mutta nykyään on hienostuneempia työkaluja, kuten HttpClient C#:ssa.

Vaihtoehtoisia tapoja ladata sivuja on monia: WebClient-luokka (nykyään vanhentunut), HttpWebRequest/Response (matalamman tason hallinta), tai kolmannen osapuolen kirjastot, kuten RestSharp tai HtmlAgilityPack.

Tärkeimmät toteutusyksityiskohdat ovat virheenkäsittely (internet-yhteyksien epävarmuus), ja asynkroninen käyttö (et blokeeraa sovelluksesi toimintaa ladatessasi).

## See Also (Katso myös):

- Microsoftin HttpClient-dokumentaatio: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- Asynkronisen ohjelmoinnin perusteet C# kielisessä ympäristössä: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/csharp/async)
- HTTP:n perusteet: [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- HtmlAgilityPack-kirjasto: [html-agility-pack.net](https://html-agility-pack.net/)