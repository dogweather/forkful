---
date: 2024-01-20 17:59:11.504816-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel er prosessen der du ber en server\
  \ om data eller utf\xF8rer en handling. Programmerere gj\xF8r dette for \xE5 integrere\
  \ med\u2026"
lastmod: '2024-03-13T22:44:40.792098-06:00'
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel er prosessen der du ber en server om data\
  \ eller utf\xF8rer en handling."
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Hva & Hvorfor?
Å sende en HTTP-forespørsel er prosessen der du ber en server om data eller utfører en handling. Programmerere gjør dette for å integrere med webtjenester, hente innhold, eller snakke med REST APIer.

## Slik gjør du:
For å sende en HTTP-forespørsel i C#, bruk `HttpClient`. Her er et enkelt eksempel:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (var client = new HttpClient())
        {
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine("Feil: " + response.StatusCode);
            }
        }
    }
}
```

Kjøring av koden henter HTML-innholdet fra `example.com` og skriver det ut eller melder ifra om en feil.

## Dypdykk
Tilbake i 2000, var `WebRequest` og `WebResponse` veien å gå for HTTP-forespørsler i .NET. Nå har `HttpClient` blitt standarden grunnet enklere kode og bedre ytelse. `HttpClient` gjenbruker TCP-tilkoblinger der det er mulig, noe som sparer ressurser. Når du jobber med REST APIer, kan du utvide bruken ved å legge til `HttpClientFactory` som håndterer levetiden på `HttpClient`-instanser dynamisk.

Alternativer? Hvis du trenger mer kontroll, kan du grave direkte inn i lavnivå `HttpWebRequest`, men dette er for det meste historie nå. I tillegg finnes det tredjepartsbiblioteker som `RestSharp` som gir et høynivålager som kan forenkle arbeidet.

Hovedpoenget: `HttpClient` er robust og fleksibelt. Det håndterer mye av kompleksiteten i nettverkskommunikasjon, men gir deg likevel stor kontroll og tilpasningsmuligheter.

## Se også
- Microsoft Docs for `HttpClient`: https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient
- En guide til `HttpClientFactory`: https://docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests
- RestSharp dokumentasjon: http://restsharp.org/
