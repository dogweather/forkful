---
date: 2024-01-20 17:43:39.363461-07:00
description: "How to: (Kuinka tehd\xE4:) Web-sivun lataamista ohjelmallisesti on tehty\
  \ l\xE4hes niin kauan kuin web on ollut olemassa. Alkuaikoina k\xE4ytettiin yksinkertaisia\u2026"
lastmod: '2024-04-05T22:51:10.728138-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Web-sivun lataamista ohjelmallisesti on tehty l\xE4hes\
  \ niin kauan kuin web on ollut olemassa."
title: Verkkosivun lataaminen
weight: 42
---

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
