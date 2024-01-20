---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å laste ned en nettside innebærer å hente dens data slik at det kan vises på en brukers enhet. Programmerere gjør dette for å behandle nettsideinnholdet på klientsiden, eksempelvis for webskraping eller offline visning.

## Hvordan:

Vi skal bruke HttpClient-klassen i .NET. Her er grunnkode for dette:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        try
        {
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();

            Console.WriteLine(responseBody);
        }
        catch(HttpRequestException e)
        {
            Console.WriteLine("\nException caught.");
            Console.WriteLine("Message : {0} ",e.Message);
        }
    }
}
```
Kjører du denne koden, vil innholdet på "http://example.com" skrives ut i konsollen. 

## Dypdykk: 

Før HttpClient, brukte programmerere WebClient eller HttpWebRequest for å laste ned nettsider. HttpClient er mer moderne og tilbyr mer funksjonalitet.

Et alternativ til HttpClient er RestSharp, en tredjeparts bibliotek som har noen funksjoner HttpClient mangler, som innebygget JSON-serialisering.

`client.GetAsync()` brukes til å sende en GET-forespørsel til den angitte Uri og returnerer en HttpResponseMessage som inneholder HTTP-responsmeldingen. Hvis du vil lese innholdet på siden, kall `response.Content.ReadAsStringAsync()`.  

## Se også


Husk at beste praksis anbefaler å gjenbruke HttpClient-objekter i stedet for å opprette nye for hver forespørsel.