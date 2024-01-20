---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Nedladdning av en webbsida innebär hämtning av data från ett specifikt URL och sparande av det på din dator. Det används av programmerare för att hämta realtidsdata, skrapa webbdata, övervaka ändringar på webbsidor, bland andra användningsområden.

## Hur man gör:

C# erbjuder `HttpClient` för att ladda ner en webbsida. Vid basisk användning:

```C#
using System;
using System.Net.Http;

class Program
{
    static async Task Main()
    {
        using HttpClient client = new HttpClient();
        string html = await client.GetStringAsync("http://example.com");
        Console.WriteLine(html);
    }
}
```

Kör programmet, det visar HTML-innehållet på "example.com".

## Djupdykning

Historiskt sett har vi sett utvecklingen från `WebClient` till `HttpWebRequest` till nuvarande `HttpClient`. `HttpClient` är mer flexibel och effektiv för modern webbsida nedladdning.

Vi har också alternativ som `RestSharp`, en populär open-source HTTP-klientbibliotek. 

Vad gäller nedladdning av en webbsida i C#, sker det i flera steg: skapa en förfrågan, sänd förfrågan, få Svaret, läs data. Genom att använda `HttpClient`, förenklas dessa steg till en enda metod: `GetStringAsync`.

## Se även

För mer detaljerade information och exempel, se följande resurser:

- [HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Making HTTP Requests](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/console-webapiclient)

För att läsa om alternativ till HttpClient:

- [RestSharp](http://restsharp.org/)