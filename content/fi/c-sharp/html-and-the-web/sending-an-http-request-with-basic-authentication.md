---
date: 2024-01-20 18:01:04.734396-07:00
description: "How to - Kuinka tehd\xE4: Perusautentikaatio on HTTP-protokollan vanhin\
  \ autentikaatiomenetelm\xE4. Se on yksinkertainen, muttei erityisen turvallinen,\
  \ sill\xE4\u2026"
lastmod: '2024-04-05T22:51:10.729197-06:00'
model: gpt-4-1106-preview
summary: "Perusautentikaatio on HTTP-protokollan vanhin autentikaatiomenetelm\xE4."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
weight: 45
---

## How to - Kuinka tehdä:
```C#
using System;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

public class BasicAuthExample
{
    private static async Task Main()
    {
        string username = "kayttaja";
        string password = "salasana";
        string url = "https://example.com/api/data";

        using (HttpClient client = new HttpClient())
        {
            // Encode the credentials and set the basic auth header
            var encoding = Encoding.UTF8.GetBytes($"{username}:{password}");
            var base64String = Convert.ToBase64String(encoding);
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", base64String);

            try
            {
                HttpResponseMessage response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            catch (HttpRequestException e)
            {
                Console.WriteLine("\nException Caught!");
                Console.WriteLine($"Message :{e.Message}");
            }
        }
    }
}
```
Sample Output:
```
{"data": "Salainen data palautettu onnistuneesti."}
```

## Deep Dive - Syväsukellus:
Perusautentikaatio on HTTP-protokollan vanhin autentikaatiomenetelmä. Se on yksinkertainen, muttei erityisen turvallinen, sillä tunnukset lähetetään base64-enkoodattuna, mikä on helppo purkaa. Siksi sitä ei tulisi käyttää sensitiivisille tiedoille ilman HTTPS-protokollaa.

Alternatiiveina on useita turvallisempia menetelmiä, kuten OAuth2, JWT (JSON Web Tokens), tai API-avaimet. Näissä menetelmissä tunnistetiedot eivät kulje selvätekstinä tai helposti purettavissa muodossa.

Perusautentikaation toteuttamisessa C#:ssa tulee huomioida, että HttpClient pitäisi olla uudelleenkäytettävä sovelluksen elinkaaren ajan. Se vähentää latencya ja resurssien kulutusta.

## See Also - Katso Myös:
- [HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Basic Authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Introduction to Authentication with ASP.NET Core](https://docs.microsoft.com/en-us/aspnet/core/security/authentication/)
- [Secure a Web API with Individual Accounts and Local Login in ASP.NET Web API 2.2](https://docs.microsoft.com/en-us/aspnet/web-api/overview/security/individual-accounts-in-web-api)
