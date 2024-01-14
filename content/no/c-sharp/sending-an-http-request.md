---
title:                "C#: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

Veldig ofte når vi bruker internett, så sender nettleseren vår en HTTP request for å få tilgang til en nettside eller ressurs. HTTP requests er averdagen av internettbruk og er nødvendig for å kommunisere med forskjellige servere. Men hvordan fungerer egentlig dette i bakgrunnen? Hvordan kan vi implementere det i våre egne programmer? I denne bloggposten skal vi se nærmere på hvordan vi kan sende en HTTP request ved hjelp av C#.

## Hvorfor
Hvis du lurer på hvorfor du ville komme i situasjoner der du må sende HTTP requests gjennom et program eller en applikasjon, kan du tenke deg eksempler som å lese data fra en API, laste ned filer eller sende data til en server. Det er en viktig del av interaksjonen med eksterne ressurser og servere. 

## Slik gjør du det
For å sende en HTTP request i C#, må vi først opprette en instans av WebClient-klassen. Deretter setter vi URL-en til den nettsiden eller ressursen vi ønsker å få tilgang til. Vi bruker deretter "DownloadString" metoden for å hente data fra URL-en og skrive det ut til konsollen. Det kan se slik ut:

```C#
using System; 
using System.Net; 

class Program { 
    static void Main(string[] args) {
        // Opprette en instans av WebClient-klassen 
        using (var client = new WebClient()) 
        {
            // Setter URL-en 
            string url = "https://example.com"; 
            //Henter data fra URL-en og skriver det ut til konsollen 
            string result = client.DownloadString(url); 
            Console.WriteLine(result); 
        } 
    } 
} 
```

Kjøringen av dette programmet vil skrive ut HTML-koden til nettsiden i konsollen. Det er viktig å huske å inkludere "using System.Net;" øverst i koden for å kunne bruke WebClient-klassen.

## Dypdykk
Nå som vi har sett på den grunnleggende implementeringen, la oss se litt nærmere på de forskjellige delene av en HTTP request. En HTTP request består av en URL (Uniform Resource Locator) som vi har brukt i eksemplet over, en HTTP-metode som spesifiserer handlingen vi ønsker å utføre (for eksempel GET, POST, PUT eller DELETE), og valgfrie parametere og data som sendes med requesten.

I tillegg til "DownloadString" metoden som vi har brukt i eksemplet, har WebClient-klassen også andre metoder som kan være nyttige i forskjellige situasjoner. For eksempel kan du bruke "UploadString" metoden hvis du trenger å sende data til en server, eller "DownloadData" metoden hvis du vil laste ned filer.

## Se også
- [Microsoft Docs: WebClient Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netcore-3.1)
- [MDN Web Docs: HTTP request methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Tutorialspoint: C# - Sending HTTP Requests](https://www.tutorialspoint.com/csharp/csharp_sending_http_requests.htm)

Forhåpentligvis har denne bloggposten gitt deg en god forståelse av hvordan du kan sende en HTTP request ved hjelp av C#. Ved å bruke WebClient-klassen, kan du enkelt få tilgang til eksterne ressurser og utveksle data med servere. Husk å alltid håndtere eventuelle feil som kan oppstå når du sender en HTTP request og åpne nye muligheter for interaksjon med internett i dine programmer.