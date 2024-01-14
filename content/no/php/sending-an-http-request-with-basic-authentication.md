---
title:                "PHP: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sende en HTTP-forespørsel med grunnleggende autentisering kan være nødvendig for å få tilgang til en API eller et passordbeskyttet nettsted. Det gir en enkel måte å autentisere brukeren sin identitet på, slik at man kan hente ut ønsket informasjon fra nettstedet.

## Hvordan
Her er et eksempel på hvordan man kan sende en HTTP-forespørsel med grunnleggende autentisering i PHP:

```PHP
<?php
// Definer variabler med brukernavn og passord
$username = "dinbruker";
$password = "dittpassord";

// Sett opp autentiseringen
$auth = base64_encode("{$username}:{$password}");

// Sett opp HTTP-headers
$headers = [
    "Authorization: Basic {$auth}"
];

// Sett opp URL og initiér en ny cURL-sesjon
$url = "https://example.com/api";
$ch = curl_init($url);

// Sett cURL-alternativer
curl_setopt($ch, CURLOPT_HTTPHEADER, $headers); // Legg til HTTP-headers
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // Returnér resultatet i stedet for å skrive det ut

// Send forespørsel og lagre resultatet
$result = curl_exec($ch);

// Skriv ut resultatet
echo $result;

// Avslutt cURL-sesjonen
curl_close($ch);
```

Dette eksempelet bruker cURL-biblioteket for å sende en HTTP-forespørsel med grunnleggende autentisering. Først definerer vi variabler med brukernavn og passord, deretter koder vi dem med base64 og legger dem til i HTTP-headers. Deretter setter vi opp en cURL-sesjon med de nødvendige alternativene og sender forespørselen til ønsket URL.

## Dypdykk
Når man sender en HTTP-forespørsel med grunnleggende autentisering, blir brukernavn og passord sendt i klartekst. Dette kan være en sikkerhetsrisiko hvis man ikke bruker HTTPS eller andre sikre tilkoblinger. Det er derfor viktig å være forsiktig med å bruke grunnleggende autentisering og alltid søke alternative metoder hvis mulig.

## Se også
- [cURL - Dokumentasjon](https://www.php.net/manual/en/book.curl.php)
- [HTTP-forespørsler med PHP - Tutorial](https://www.php.net/manual/en/ref.curl.php)
- [Sikkerhet ved bruk av grunnleggende autentisering](https://www.owasp.org/index.php/Basic_Authentication)