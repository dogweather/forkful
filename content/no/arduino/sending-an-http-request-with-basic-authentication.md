---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Arduino: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å sende en HTTP forespørsel med grunnleggende autentisering er en måte å kommunisere med en webtjeneste eller nettside på ved å legge til en autentiseringsnøkkel i forespørselen. Dette brukes ofte av programmerere for å få tilgang til beskyttet informasjon eller utføre handlinger på en nettside. 

# Hvordan:

Arduino er et kraftig verktøy for å kommunisere med eksterne tjenester ved å bruke HTTP forespørsler. For å sende en HTTP forespørsel med grunnleggende autentisering, kan du følge disse trinnene:

```
Arduino http.begin(URL); // Angi nettadressen du vil bruke
http.setAuthorization("username", "password"); // Legg til ditt brukernavn og passord
http.addHeader("Content-Type", "text/html"); // Legg til innholdstypen du vil bruke
int httpCode = http.GET(); // Lag en GET forespørsel

if (httpCode > 0) { // Sjekk om forespørselen var vellykket
  String payload = http.getString(); // Hent svaret fra serveren
  Serial.println(httpCode); // Skriv ut statuskoden
  Serial.println(payload); // Skriv ut svaret
}
http.end(); // Avslutt forespørselen
```

For å kunne sende en HTTP forespørsel med grunnleggende autentisering, må du ha et gyldig brukernavn og passord som er gitt av serveren du kommuniserer med. Du kan også endre nettadressen og innholdstypen basert på ditt spesifikke behov.

Et eksempel på en vellykket HTTP forespørsel med grunnleggende autentisering kan se slik ut:

```
200
<html lang="en">
<head>
<title>Arduino Tutorial</title>
</head>
<body>
<h1>Welcome to our tutorial on sending HTTP requests!</h1>
</body>
</html>
```

# Dypdykk:

Dette konseptet ble først introdusert i HTTP 1.0 standarden i 1996, for å gi en enkel metode for å autentisere brukere og sikre HTTP forespørsler. I dag er grunnleggende autentisering fortsatt en vanlig måte for programmerere å få tilgang til beskyttet informasjon eller utføre handlinger på en nettside. Andre alternativer for autentisering inkluderer Digest, OAuth og API nøkler.

Når du sender en HTTP forespørsel med grunnleggende autentisering, koder Arduino automatisk brukernavnet og passordet ved hjelp av base64 algoritmen. Dette sikrer at dine autentiseringsdetaljer ikke kan leses av uautoriserte personer under overføringen.

# Se også:

Lær mer om HTTP autentisering ved å besøke disse nettstedene:

- [HTTP Basic Authentication: What It Is and How to Use It](https://swagger.io/docs/specification/authentication/basic-authentication/)
- [HTTPS for Embedders, Part 3: Client Authentication Basics](https://jwkimura.medium.com/https-for-embedders-part-3-client-authentication-basics-997ea0abe8c)