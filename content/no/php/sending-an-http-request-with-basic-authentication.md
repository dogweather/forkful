---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "PHP: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering er en måte å sikre at bare autoriserte brukere får tilgang til et nettsted eller en webtjeneste. Dette er spesielt viktig når man ønsker å beskytte følsom informasjon, som brukernavn og passord. Programmerere bruker denne metoden for å sikre sine nettsted eller applikasjoner og hindre uønskede brukere fra å få tilgang til sensitive data.

# Slik gjør du det:
```PHP
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, $url); // angi URL-en som skal sendes en forespørsel til
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // returner responsen i stedet for å skrive den ut
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC); // angi autentiseringsmetoden
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password"); // sett brukernavn og passord

$result = curl_exec($ch); // send forespørselen og lagre responsen

curl_close($ch); // lukk curl-ressursen

echo $result; // skriv ut responsen
```

**Eksempel output:**
```
Dette er responsen fra nettstedet eller applikasjonen.
```

# Dypdykk:
**Historisk kontekst:**
HTTP-forespørsler med grunnleggende autentisering har lenge vært en vanlig måte å sikre nettsteder og applikasjoner på. Metoden ble først introdusert i HTTP 1.0-standarden i 1996, og har siden blitt brukt av utviklere over hele verden.

**Alternativer:**
Selv om grunnleggende autentisering er en enkel og effektiv metode, er det viktig å merke seg at brukernavn og passord sendes i klartekst og derfor er sårbare for ondsinnede angrep. En mer sikker alternativ er å bruke OAuth-autentisering, som lar brukeren autorisere tilgang til deres konto fra en tredjepartsapplikasjon uten å måtte dele deres faktiske brukernavn og passord.

**Implementeringsdetaljer:**
For å kunne sende en HTTP-forespørsel med grunnleggende autentisering, må du først kjenne til URL-en du vil sende forespørselen til og ha et gyldig brukernavn og passord for å autentisere deg. Deretter må du bruke cURL-funksjonen i PHP for å sette nødvendige parametere og sende forespørselen.

# Se også:
- [PHP's offisielle dokumentasjon for cURL](https://www.php.net/manual/en/book.curl.php)
- [Introduksjon til HTTP-autentisering](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)