---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP forespørsel er prosessen med å be en spesiell ressurs (som en HTML-fil, bilde eller annet) fra en server. Programmerere gjør dette for å hente, slette, oppdatere eller sende informasjon til en server.

## Hvordan:

Her er et grunnleggende PHP-eksempel på en GET-forespørsel ved hjelp av cURL:

```PHP
<?php
// Initialiserer en ny sesjon og returnerer cURL håndtaket til bruk med curl_setopt()
$ch = curl_init("http://example.com");

// Setter en cURL overføring mulighet
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

// Utfører en cURL økten du har håndtaket
$output = curl_exec($ch);

// Lukk en cURL økt 
curl_close($ch);

// Vis utdata
echo $output;
?>
```

I output, vil du få innholdet på http://example.com

## Dypdykk

### Historisk kontekst
HTTP-forespørsler er en grunnleggende del av moderne nettbrowsing og ble definert tidlig i webens historie av Tim Berners-Lee ved CERN.  

### Alternativer
Selv om cURL er veldig vanlig, er det andre biblioteker å vurdere for å sende HTTP-forespørsler i PHP, som GuzzleHttp og HTTPful.

### Implementasjonsdetaljer
For det første, når du bruker cURL, er det viktig å alltid lukke cURL ressursen etter bruk for å frigjøre systemressurser. Dernest, hvis webserveren du henter fra bruker noe enn standard port (80 for http og 443 for https), må du spesifisere den i URL-en.

## Se også:

- Lær mer om PHP cURL biblioteket i [PHP dokumentasjonen](https://www.php.net/manual/en/book.curl.php)
- Les hva [Mozilla har å si](https://developer.mozilla.org/no/docs/Web/HTTP/Overview) om HTTP.
- Bli kjent med alternative biblioteker: [GuzzleHttp](http://docs.guzzlephp.org/en/stable/), [HTTPful](http://nategood.com/httpful/)