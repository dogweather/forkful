---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
aliases:
- /sv/c-sharp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:24.664117-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
I C# skickar vi HTTP-begäran med grundläggande autentisering för att säkert överföra användarnamn och lösenord till servern. Detta används för att bevisa användarens identitet och få tillgång till skyddade resurser.

## Steg för Steg:
Vi använder klassen `HttpClient` och lägger till en header för autentisering. Lägg märke till hur enkelt det är:

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        var credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("användarnamn:lösenord"));
        using (var client = new HttpClient())
        {
            client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", credentials);

            var response = await client.GetAsync("http://exempel.se/data");
            var content = await response.Content.ReadAsStringAsync();
            Console.WriteLine(content);
        }
    }
}
```

Om servern accepterar dina referenser, får du tillbaka svaret, annars ett 401 Unauthorized fel.

## Djupdykning:
Grundläggande autentisering har använts sedan HTTP/1.0 och är en enkel metod för att skicka användarnamn och lösenord, kodat i Base64. Det anses inte som särskilt säkert eftersom Base64 är lätt att avkoda om inte en säker förbindelse som HTTPS används.

Alternativ till grundläggande autentisering inkluderar OAuth, Token-baserad autentisering, och API-nycklar, vilka alla erbjuder mer säkra och flexibla lösningar.

En viktig implementationsdetalj är att se till att använda HTTPS när du skickar känslig information. För att ytterligare öka säkerheten kan du implementera mer avancerade autentiseringssystem som nämnts ovan.

## Se även:
- [HttpClient-klassen på Microsofts Dokumentation](https://learn.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [HTTP-autentisering på MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [En säkerhetsguide till grundläggande autentisering med C#](https://www.codeproject.com/Articles/1768/Basic-Authentication-on-a-WCF-REST-Service)
