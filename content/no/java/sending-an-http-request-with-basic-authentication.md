---
title:                "Send en http-forespørsel med grunnleggende autentisering"
html_title:           "Java: Send en http-forespørsel med grunnleggende autentisering"
simple_title:         "Send en http-forespørsel med grunnleggende autentisering"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Sending av HTTP-forespørsler med grunnleggende autentisering er en vanlig måte for programmerere å sikre at bare autoriserte brukere kan få tilgang til deres nettapplikasjoner. Dette gjøres ved å inkludere en brukernavn og et passord i HTTP-forespørselen, som deretter valideres av serveren før tilgang til applikasjonen gis.

Hvordan:
```
Java URL objekt = new URL ("http://www.example.com");
HttpURLConnection httpCon = (HttpURLConnection) objekt.openConnection();
httpCon.setRequestProperty("Authorization", "Basic " +                       "brukernavn" + ":" + "passord");
int responsKode = httpCon.getResponseCode();
System.out.println(responsKode);
```

For å sende en HTTP-forespørsel med grunnleggende autentisering i Java, må vi først opprette et URL-objekt som refererer til nettadressen til applikasjonen vi ønsker å koble til. Deretter oppretter vi en HttpURLConnection som brukes til å åpne en forbindelse til denne URL-en. For å sende brukernavnet og passordet, bruker vi setRequestProperty() metoden og inkluderer dem i form av en base64-kodet streng. Til slutt kan vi få responskoden fra serveren, som vil være 200 hvis autentiseringen var vellykket.

Dypdykk:
HTTP-forespørsler med grunnleggende autentisering har vært i bruk siden 1990-tallet og er fortsatt en av de enklere metodene for autentisering over nettet. Mens det sikrer at bare autoriserte brukere får tilgang, er det viktig å merke seg at denne metoden ikke krypterer brukernavn og passord, så det er fortsatt en risiko for at de kan bli kompromittert.

Alternativer inkluderer mer avanserte autentiseringsmetoder som JWT (JSON Web Token) og OAuth, som gir bedre sikkerhet og funksjonalitet for autentisering. Implementering av en HTTP-forespørsel med grunnleggende autentisering krever også at applikasjonen vår støtter dette, slik at brukernavn og passord kan hentes fra forespørselen og valideres mot riktig oppbevaring.

Se også:
- Java URL klasse dokumentasjon: https://docs.oracle.com/javase/7/docs/api/java/net/URL.html
- HTTP grunnleggende autentisering på MDN Web Docs: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme