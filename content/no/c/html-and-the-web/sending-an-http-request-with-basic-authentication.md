---
title:                "Sende en HTTP-forespørsel med grunnleggende autentisering"
date:                  2024-02-03T18:09:15.243583-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sende en HTTP-forespørsel med grunnleggende autentisering"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering i C involverer å lage en HTTP-forespørsel som inkluderer en autorisasjonsheader med brukerlegitimasjon kodet i Base64. Dette er en vanlig metode for å legge til et enkelt autentiseringslag til HTTP-forespørsler, som gjør det mulig å programmelt få tilgang til begrensede ressurser.

## Hvordan:
For å sende en HTTP-forespørsel med grunnleggende autentisering i C, trenger vi å bruke libcurl-biblioteket, et populært, allsidig og lett å bruke klient-side URL-overføringsbibliotek. Det håndterer diverse protokoller, inkludert HTTP og HTTPS, noe som gjør oppgaven vår enklere. Forsikre deg om at libcurl er installert på systemet ditt før du fortsetter. Her er et grunnleggende eksempel som demonstrerer hvordan du sender en GET-forespørsel med grunnleggende autentisering:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    hvis(curl) {
        // URLen som forespørselen sendes til
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Aktiverer bruk av grunnleggende autentisering
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Oppgir brukernavn og passord for grunnleggende autentisering
        curl_easy_setopt(curl, CURLOPT_USERPWD, "brukernavn:passord");

        // Utfører GET-forespørselen
        res = curl_easy_perform(curl);

        // Sjekker for feil
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() feilet: %s\n",
                    curl_easy_strerror(res));

        // Alltid rydde opp
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
I eksemplet ovenfor, erstatt `"http://example.com/resource"`, `"brukernavn"`, og `"passord"` med din faktiske URL, brukernavn og passord.

Denne koden initialiserer et `CURL` objekt, setter URLen, aktiverer HTTP Grunnleggende Autentisering, og spesifiserer legitimasjonen. Deretter sender den forespørselen og rydder opp etter seg selv. Hvis det lykkes, hentes den forespurte ressursen; hvis det er en feil, skrives den ut til stderr.

Eksempelutdata (gitt vellykket autentisering og tilgang til ressurs) vil kanskje ikke bli direkte vist av programmet, da eksemplet hovedsakelig demonstrerer sending av forespørselen. For å skrive ut responsen, ville du utvide programmet til å håndtere HTTP-responsdata.

## Dypdykk:
Å sende HTTP-forespørsler med grunnleggende autentisering i C, som vist, utnytter libcurl-biblioteket for dets robusthet og enkelhet. Historisk sett var det å lage HTTP-forespørsler rent i C uten slike biblioteker tungvint og feilutsatt, og involverte lavnivå sokkelprogrammering og manuell konstruksjon av HTTP-headers.

Grunnleggende autentisering selv er en metode fra de tidlige dagene av weben. Det sender legitimasjon i et lett dekoderbart format (Base64), som er iboende usikkert over klartekstkanaler. Moderne applikasjoner foretrekker ofte mer sikre autentiseringsmetoder, som OAuth 2.0 eller JWT (JSON Web Tokens), spesielt for sensitiv data.

Likevel, for interne, mindre kritiske systemer, eller raske og skitne skript hvor bekvemmelighet veier tyngre enn sikkerhetsbekymringer, forblir grunnleggende autentisering i bruk. Videre, når kombinert med krypterte tilkoblinger (HTTPS), blir dens enkelhet en fordel for rask utvikling, testing, eller automatisering hvor høyere sikkerhetsmekanismer ikke er like nødvendig.

I kontekster hvor banebrytende sikkerhet er ikke-forhandlingsbart, bør alternativer som token-basert autentisering prioriteres. Likevel, å forstå hvordan man implementerer grunnleggende autentisering i C gjennom libcurl gir en grunnleggende ferdighet som kan tilpasses til ulike autentiseringsmetoder og protokoller, noe som reflekterer de nyanserte avveiningene mellom sikkerhet, bekvemmelighet, og applikasjonskrav i webutvikling.
