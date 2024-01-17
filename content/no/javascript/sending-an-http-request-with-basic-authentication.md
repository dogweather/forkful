---
title:                "Sending en http-forespørsel med grunnleggende autentisering"
html_title:           "Javascript: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Sending av HTTP-forespørsler med grunnleggende autentisering er en vanlig praksis for programmere. Dette er et måte å validere brukeren sin identitet på og sikre at bare autoriserte personer har tilgang til visse tjenester eller ressurser. Det er vanligvis brukt i API-kall eller nettleserbaserte applikasjoner.

# Hvordan:
For å sende en HTTP-forespørsel med grunnleggende autentisering i Javascript, må du først opprette en instans av XMLHttpRequest-objektet. Deretter må du sette metoden til "GET" eller "POST", avhengig av hva slags forespørsel du vil sende. Så må du sette URLen til hvor forespørselen skal sendes, og legge til etautentiseringsheader ved hjelp av "setRequestHeader" -metoden med "Authorization" som parameter. Til slutt må du sende forespørselen ved hjelp av "send" -metoden. En implementasjon kan se slik ut:

```Javascript
var xhr = new XMLHttpRequest();

xhr.open("GET", "https://example.com/resource"); 
xhr.setRequestHeader("Authorization", "Basic " + btoa(username + ":" + password));
xhr.send();
```

##Forventet Output:
Dette avhenger av hva som er forespurt, men hvis forespørselen er vellykket, vil responsen være en statuskode 200 og eventuelle data som er returnert av serveren.

#Dypdykk:
HTTP-grunnleggende autentisering er en enkel måte å validere brukere på, men det er viktig å merke seg at brukernavn og passord som sendes i klartekst, kan potensielt bli fanget opp av uautoriserte personer. Derfor er det mer sikre autentiseringsmetoder tilgjengelig som OAuth og JWT. I tillegg er den nåværende versjonen av Javascript, ES6, introdusert Fetch API som tilbyr en mer moderne og enklere tilnærming til å sende HTTP-forespørsler.

#Se også:
- MDN Web Docs: Basic authentication over HTTP
https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme
- Google Developers: Working with HTTP basic authentication in Javascript
https://developers.google.com/web/updates/2017/01/basic-auth-and-forms 
- W3Schools: XMLHttpRequest Set Request Header Method
https://www.w3schools.com/xml/ajax_xmlhttprequest_setrequestheader.asp