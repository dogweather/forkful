---
title:                "Å laste ned en nettside"
html_title:           "C#: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du besøker en nettside, lastes den ned på datamaskinen din slik at du kan se og interagere med innholdet. Men hva om du vil laste ned hele nettsiden og lagre den for senere bruk? I denne artikkelen skal vi se på hvordan du kan gjøre akkurat det ved hjelp av C#.

## Hvordan

For å laste ned en nettside, trenger vi å bruke et webklientobjekt i C#. Dette er en del av .NET Framework og lar oss kommunisere med nettressurser.

Vi kan først opprette et webklientobjekt ved å inkludere System.Net namespace i koden vår. Deretter kan vi bruke webklienten til å laste ned nettsiden ved å bruke WebClient.DownloadFile() metoden. La oss se på et eksempel:

```C#
using System;
using System.Net;

WebClient client = new WebClient();
client.DownloadFile("https://www.examplewebsite.com", "example.html");

//Dette vil laste ned nettsiden og lagre den som en HTML-fil med navnet "example.html" på datamaskinen din.
```

Vi kan også laste ned en nettside som et tekststreng ved hjelp av WebClient.DownloadString() metoden. Dette er nyttig hvis vi vil behandle teksten videre i koden vår. Her er et eksempel:

```C#
using System;
using System.Net;

WebClient client = new WebClient();
string webpage = client.DownloadString("https://www.examplewebsite.com");

//Dette vil laste ned nettsiden som en tekststreng og lagre den i variabelen "webpage".
```

Nå har vi lastet ned nettsiden, men hva om vi vil laste ned bilder eller andre filer som ligger på nettsiden? Vi kan bruke WebClient.DownloadFile() metoden til å laste ned alle ressurser på nettsiden. Dette kalles også å "lytte" på en nettside. Her er et eksempel:

```C#
using System;
using System.Net;

WebClient client = new WebClient();
client.DownloadFile("https://www.examplewebsite.com", "example.html");

//Dette vil laste ned nettsiden og alle ressurser som bilder og videoer, og lagre dem på datamaskinen din.
```

Det er viktig å merke seg at når du laster ned en nettside på denne måten, vil den beholden sin originale formatering og koding. Det kan være lurt å konvertere filen til et annet format, som for eksempel UTF-8, før du bruker den i koden din.

## Deep Dive
Å laste ned en nettside kan være nyttig for mange forskjellige formål. Kanskje du vil lagre en kopi av en nettside for senere bruk, eller kanskje du vil analysere innholdet på nettsiden for å trekke ut spesifikk informasjon. Ved å bruke webklientobjektet i C#, har du enkelt tilgang til å laste ned nettsider og utføre ulike operasjoner på dem i koden din.

## Se Også
- [Official documentation for WebClient class in C#](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netframework-4.8)
- [Tutorial on downloading files in C#](https://www.dotnetperls.com/downloadstring)
- [C# Tutorials in Norwegian](https://www.tutlane.com/tutorial/csharp)