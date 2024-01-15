---
title:                "Sending en http-forespørsel med enkel godkjenning"
html_title:           "C#: Sending en http-forespørsel med enkel godkjenning"
simple_title:         "Sending en http-forespørsel med enkel godkjenning"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du vil kommunisere med en API eller en server som krever autentisering, vil du måtte sende en HTTP-forespørsel med basic authentication for å bekrefte identiteten din. Dette er en vanlig praksis innenfor webutvikling og programmering, og det er viktig å forstå hvordan dette fungerer i C#.

## Hvordan
For å sende en HTTP-forespørsel med basic authentication i C#, må du først opprette en instans av WebClient-klassen og sette autorisasjonsheaderen til Basic. Deretter må du kode innloggingsinformasjonen din i Base64 og sende den som en del av autorisasjonsheaderen.

```C#
// Opprett en instans av WebClient-klassen
WebClient client = new WebClient();

// Sett autorisasjonsheaderen til Basic
client.Headers["Authorization"] = "Basic";

// Koding av brukernavn og passord til Base64
string credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("brukernavn:passord"));

// Send Base64-kodede innloggingsinformasjonen i autorisasjonsheaderen
client.Headers["Authorization"] += credentials;

// Send en HTTP-forespørsel til en spesifikk URL
string response = client.DownloadString("https://www.example.com/api");

// Utskrift av svar fra serveren
Console.WriteLine(response);
```

Når du kjører koden over, skal den koble til API-en eller serveren og returnere svaret som en streng.

## Deep Dive
HTTP-forespørsler med basic authentication fungerer ved å sende et Base64-kodet brukernavn og passord som en del av autorisasjonsheaderen i en forespørsel til en server. Serveren vil da dekode og verifisere informasjonen og gi tilgang til forespurte ressurser hvis alt stemmer.

Det er viktig å merke seg at basic authentication ikke er en komplett sikkerhetsløsning, og bør ikke brukes til å beskytte sensitiv informasjon. Det anbefales å bruke HTTPS i tillegg for å kryptere forespørsler og sikre autentisering.

## Se Også
- [WebClient Class (System.Net)](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=net-5.0)
- [Basic Authentication in C#](https://www.geeksforgeeks.org/basic-authentication-in-c-sharp/)
- [HTTP Basic Authentication](https://www.inetdaemon.com/tutorials/internet/http/authentication/basic.shtml)