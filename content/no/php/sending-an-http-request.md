---
title:                "Sending en HTTP forespørsel"
html_title:           "PHP: Sending en HTTP forespørsel"
simple_title:         "Sending en HTTP forespørsel"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel betyr å be om informasjon fra en nettside eller webserver. Dette er en vanlig oppgave for programmerere når de vil hente data fra et annet nettsted eller nettjeneste. Det kan være for å integrere funksjonalitet fra en tredjeparts tjeneste, eller for å hente oppdateringer fra en ekstern kilde.

## Slik gjør du det:

Å sende en HTTP-forespørsel i PHP er enkelt og kan gjøres med funksjonen ```file_get_contents()```. Med denne kan du sende en forespørsel og få tilbake svaret som en streng. Her er et eksempel på å hente informasjon fra en API-tjeneste og vise resultatet:

```PHP
$json = file_get_contents('https://api.example.com/user/123');
$obj = json_decode($json);
echo 'Brukernavn: ' . $obj->username;
echo 'E-post: ' . $obj->email;
```

Dette vil hente data om bruker nummer 123 og vise brukernavn og e-postadresse.

## Dypdykk

I tidligere versjoner av PHP var det vanlig å bruke funksjonene ```fopen()``` og ```fgets()``` for å sende en HTTP-forespørsel og lese svaret. Dette var en mer omstendelig og upraktisk måte, og derfor ble ```file_get_contents()``` introdusert. Det finnes også andre alternativer for å sende HTTP-forespørsler, som for eksempel biblioteker som cURL og Guzzle.

Når du sender en HTTP-forespørsel med ```file_get_contents()```, er standardmetoden GET, men du kan også spesifisere andre metoder som POST og PUT. Du kan også legge til tilpassede HTTP-headerfelt for å sende ekstra informasjon med forespørselen.

## Se også

- Dokumentasjon for ```file_get_contents()```: https://www.php.net/manual/en/function.file-get-contents.php
- Alternativet cURL: https://www.php.net/manual/en/book.curl.php
- Alternativet Guzzle: https://docs.guzzlephp.org/