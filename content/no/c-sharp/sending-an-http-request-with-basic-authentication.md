---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Å sende en HTTP-forespørsel med Basic Authentication i C#

## Hva & Hvorfor?
Vi sender en HTTP forespørsel med grunnleggende autentisering for å interagere med en netttjeneste som krever brukeridentifikasjon. Dette lar programmerere tilgang til beskyttede ressurser.

## Hvordan:
For å sende en HTTP forespørsel med Basic Authentication i C#, kan vi bruke `HttpClient` klassen, som er inkludert i `System.Net.Http` navneområdet. Her er et eksempel:

```C#
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using HttpClient client = new HttpClient();
        string userName = "username";
        string password = "password";

        string encoded = Convert.ToBase64String(Encoding.GetEncoding("ISO-8859-1").GetBytes(userName + ":" + password));
        client.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", encoded);

        HttpResponseMessage response = await client.GetAsync("http://example.com");
        string responseString = await response.Content.ReadAsStringAsync();
        
        Console.WriteLine(responseString);
    }
}
```
I ovenstående kode, setter vi `Authorization` hodet til en `Basic` autentiseringstreng bestående av brukernavn og passord kodet til Base64.

## På Dypet
Historisk sett, ble Basic Authentication implementert som en del av HTTP/1.0-standarden i 1996. Selv om det er en gammel teknikk, forblir den relevant og mye brukt.

Det er alternativer til Basic Authentication. Bearer token (oftest brukt med OAuth 2.0) og Digest Access Authentication er to av dem. Valget mellom disse teknikkene avhenger av bruksområdet.

En viktig ting å merke seg når vi sender en HTTP-forespørsel med Basic Authentication er at brukernavn og passord blir sent som ren tekst kodet i Base64. Dette betyr at hvis kommunikasjonen ikke er sikret med HTTPS, så kan den dekodes og lese av ondsinnede parter.

## Se også:
1. [HttpClient Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
2. [Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
3. [Other HTTP Authentication Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)