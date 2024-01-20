---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att skicka data via HTTP protokollet, men skyddar data genom att kräva autentiseringsuppgifter. Programmerare gör detta för att skydda känsliga data och begränsa åtkomsten till en webbresurs.

## Hur gör man det:
Här är ett enkelt exempel på en C#-kodsnutt som visar hur man skickar en HTTP-förfrågan med grundläggande autentisering.

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    private static async Task Main()
    {
        var client = new HttpClient();

        var byteArray = Encoding.ASCII.GetBytes("username:password");
        client.DefaultRequestHeaders.Authorization = 
            new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

        var result = await client.GetAsync("http://example.com");

        Console.WriteLine(result.StatusCode);
    }
}
```

När du kör programmet, ska du se statuskoden för din GET-förfrågan skrivna ut i konsolen.

## Djupdykning
Grundläggande autentisering är en av de första autentiseringsmetoderna som har använts inom webbutveckling. Det är en okomplicerad metod, men det har vissa svagheter. Det är till exempel inte krypterat, vilket betyder att det inte är säkert mot avlyssningsattacker.

Alternativ till grundläggande autentisering inkluderar Digest autentisering, token-baserad autentisering och OAuth. Dessa metoder är mer komplexa att implementera men erbjuder bättre säkerhet.

Under huven konverterar HttpClient klassen i C# först 'username:password' strängen till en ASCII-byte array. Detta konverteras sedan till en Base64-sträng och kopplas med 'Basic' för att skapa 'Authorization' headern i HTTP-förfrågan.

## Se även
För mer information om HTTP och autentisering, se dessa källor:

1. [Mozilla Developer Network: HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [Microsoft Tutorial: Consume a Web API from .NET](https://docs.microsoft.com/en-us/ado-net/data-services/consume/web-api-client)
3. [Microsoft Docs: HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)