---
aliases:
- /nl/c-sharp/sending-an-http-request-with-basic-authentication/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:47.213583-07:00
description: "We sturen een HTTP-verzoek met basisauthenticatie om toegang te krijgen\
  \ tot beschermde bronnen door gebruikersgegevens in de aanvraagheader op te nemen.\u2026"
lastmod: 2024-02-18 23:09:01.852045
model: gpt-4-0125-preview
summary: "We sturen een HTTP-verzoek met basisauthenticatie om toegang te krijgen\
  \ tot beschermde bronnen door gebruikersgegevens in de aanvraagheader op te nemen.\u2026"
title: Een HTTP-verzoek verzenden met basisauthenticatie
---

{{< edit_this_page >}}

## Wat & Waarom?
We sturen een HTTP-verzoek met basisauthenticatie om toegang te krijgen tot beschermde bronnen door gebruikersgegevens in de aanvraagheader op te nemen. Programmeurs gebruiken dit voor eenvoudige authenticatiesystemen, voornamelijk waar een snelle en eenvoudige oplossing geschikt is.

## Hoe te:
Laten we direct duiken in wat code. Hieronder vind je een minimale voorbeeldcode die C# gebruikt om een HTTP-verzoek met basisauthenticatie te versturen:

```C#
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        using (var client = new HttpClient())
        {
            var inloggegevens = Convert.ToBase64String(Encoding.ASCII.GetBytes("gebruikersnaam:wachtwoord"));
            client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Basic", inloggegevens);

            HttpResponseMessage response = await client.GetAsync("http://jouwapi.com/beschermd");

            if (response.IsSuccessStatusCode)
            {
                string responseBody = await response.Content.ReadAsStringAsync();
                Console.WriteLine(responseBody);
            }
            else
            {
                Console.WriteLine($"Error: {response.StatusCode}");
            }
        }
    }
}
```
Run dit, en als je eindpunt en inloggevens correct zijn, krijg je de bron. Zo niet, dan zie je een foutstatuscode.

## Diepere Duik
Basis Authenticatie is oud, echt oud, teruggaand naar de vroege dagen van het internet. Het is eenvoudig: base64-encodeer "gebruikersnaam:wachtwoord" en plak het op de `Authorization` header.

Er zijn alternatieven met strengere beveiliging: OAuth2, API-sleutels of JWT-tokens. Basis Auth is nog steeds in gebruik vanwege zijn eenvoud, maar let op, het is niet versleuteld en kan onderschept worden als het niet over HTTPS wordt gebruikt.

Wanneer je deze methode gebruikt, houd dan in gedachten:
- Gebruik altijd HTTPS om de gegevens tijdens het transport te beschermen.
- Het is een beetje alsof je je huissleutel onder de mat achterlaat – gemakkelijk maar kwetsbaar. Gebruik het dus voor scenario's met een laag risico.
- Aangezien de inloggegevens bij elke aanvraag worden meegezonden, is het niet de meest efficiënte methode voor drukke systemen.

## Zie Ook
- [Microsofts HttpClient Klassen Documentatie](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Mozilla's Uitleg over Basisauthenticatie](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [OWASP Authenticatie Cheatsheet](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)
