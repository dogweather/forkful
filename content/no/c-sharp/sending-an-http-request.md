---
title:                "Sending en http forespørsel"
html_title:           "C#: Sending en http forespørsel"
simple_title:         "Sending en http forespørsel"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor
HTTP-forespørsler er en viktig del av å kommunisere med nettet. I C# kan du bruke HTTP-forespørsler til å få tilgang til eksterne ressurser og data, noe som er nyttig for å lage dynamiske og interaktive programmer.

## Hvordan Sende en HTTP Request
Det første du trenger å gjøre er å legge til `System.Net.Http`-biblioteket i prosjektet ditt. Dette gir deg tilgang til alle klassene som trengs for å sende en HTTP-forespørsel.

Deretter kan du bruke `HttpClient`-klassen til å opprette en ny instans av en HTTP-klient. Dette vil være utgangspunktet for alle dine forespørsler. For eksempel kan du skrive:

```
var client = new HttpClient();
```

Neste steg er å opprette en `HttpRequestMessage`-instans som inneholder informasjon om forespørselen din. Dette inkluderer typen forespørsel (GET, POST, PUT, etc.), URL-adressen og eventuelle nødvendige autentiseringsdetaljer. Et eksempel på dette kan være:

```
var request = new HttpRequestMessage(HttpMethod.Get, "https://www.example.com/api/users");
```

Nå kan du faktisk sende forespørselen ved hjelp av HTTP-klienten du opprettet tidligere. Du kan gjøre dette ved å bruke `SendAsync`-metoden og lagre resultatet i en `HttpResponseMessage`-instans. For eksempel:

```
var response = await client.SendAsync(request);
```

Til slutt kan du hente dataene fra svaret ved å bruke `Content`-egenskapen til `HttpResponseMessage`. Dette vil vanligvis være i JSON-format, og du kan enkelt konvertere det til C#-objekter ved hjelp av `JsonConvert`-klassen fra `Newtonsoft.Json`-biblioteket.

## Dypere Dykk
En viktig del av å sende HTTP-forespørsler er å forstå HTTP-statuskoder og hvordan du kan håndtere dem i koden din. Her er noen av de vanligste statuskodene du kan støte på:

- 200 OK: Dette betyr at forespørselen var vellykket og at dataene du ba om ble returnert.
- 401 Unauthorized: Dette betyr at forespørselen ikke var autorisert, og du må kanskje legge til autentiseringsdetaljer i forespørselen din.
- 404 Not Found: Dette betyr at ressursen du ba om ikke kunne bli funnet.

I tillegg kan det være nyttig å bruke debugging-verktøy som Postman for å teste og feilsøke dine HTTP-forespørsler.

## Se Også
- [HttpClient Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [HttpRequestMessage Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httprequestmessage)
- [HttpResponseMessage Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpresponsemessage)