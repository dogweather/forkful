---
date: 2024-01-20 18:01:24.664117-07:00
description: "I C# skickar vi HTTP-beg\xE4ran med grundl\xE4ggande autentisering f\xF6\
  r att s\xE4kert \xF6verf\xF6ra anv\xE4ndarnamn och l\xF6senord till servern. Detta\
  \ anv\xE4nds f\xF6r att bevisa\u2026"
lastmod: '2024-03-13T22:44:37.912996-06:00'
model: gpt-4-1106-preview
summary: "I C# skickar vi HTTP-beg\xE4ran med grundl\xE4ggande autentisering f\xF6\
  r att s\xE4kert \xF6verf\xF6ra anv\xE4ndarnamn och l\xF6senord till servern."
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

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
