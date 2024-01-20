---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvordan og Hvorfor?
Å sende en HTTP-forespørsel er prosessen med å overføre bestemt informasjon til en server eller nettressurs via HTTP-protokollen. Vi programmerere gjør dette for å motta data, sende data, eller manipulere en tilstand på en server.

## Hvordan å:
Her er en enkel kode for å sende en GET-forespørsel ved hjelp av HttpClient i C#:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        HttpResponseMessage response = await client.GetAsync("http://example.com");
        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();

        Console.WriteLine(responseBody);
    }
}
```

Denne koden vil sende en GET-forespørsel til "http://example.com" og så skrive ut kroppen til serverens respons.

## Dypdykk
Historisk sett har mange HTTP-klienter blitt brukt i .NET Framework, som HttpWebRequest og WebClient. HttpClient, introdusert i .NET Framework 4.5, tilbyr en mer moderne og fleksibel tilnærming. 

Som et alternativ, kan du bruke RestSharp, et populært open-source prosjekt. RestSharp tilbyr mange funksjoner og er lett å bruke, men generelt sett, er HttpClient mer enn nok for de fleste oppgaver.

Når du bruker HttpClient, sørg for å gjenbruke HttpClient-instanser så langt det er mulig. HttpClient er ment å være instantiert en gang og gjenbrukt gjennom hele livsløpet til en applikasjon. 

## Se også 
* [RestSharp-prosjekt](https://restsharp.dev/)
* [HttpWebRequest vs HttpClient vs WebClient](https://www.infoworld.com/article/3044133/httpwebrequest-vs-httpclient-vs-webclient.html)