---
title:                "Sending en http-forespørsel med grunnleggende autentisering"
html_title:           "PHP: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sende en HTTP-forespørsel med grunnleggende autentisering er en enkel og sikker måte å sikre at kun autoriserte brukere har tilgang til en bestemt nettside eller ressurs. Det er særlig nyttig når man ønsker å beskytte sensitiv informasjon eller begrense tilgang til en spesifikk gruppe brukere.

## Hvordan
For å sende en HTTP-forespørsel med grunnleggende autentisering i PHP, kan du bruke funksjonen `file_get_contents()` sammen med en `stream_context` for å legge til autentiseringsopplysninger i forespørselen. For eksempel:

```PHP
$username = "brukernavn";
$password = "passord";

$opts = array(
    'http' => array(
        'method' => "GET",
        "header" => "Authorization: Basic " . base64_encode("$username:$password")
    )
);

$context = stream_context_create($opts);
$response = file_get_contents("https://example.com", false, $context);
echo $response;
```

Dette vil sende en GET-forespørsel til `https://example.com` med brukernavn og passord i autentiseringsheaderen. Hvis forespørselen er vellykket, vil svaret fra nettsiden bli skrevet ut.

## Deep Dive
Grunnleggende autentisering er en enkel autentiseringsmetode som krever brukernavn og passord i klartekst for å få tilgang til en ressurs. Når en forespørsel er gjort, vil informasjonen bli kodet med Base64 og sendt i headeren som viser at forespørselen er autorisert. Selv om dette kan føles som en sikker måte å autentisere brukere på, bør det bemerkes at Base64-koding ikke er en egentlig krypteringsmetode og at andre autentiseringsmetoder som OAuth kan være mer sikre.

## Se også
- [PHP: basic authentication](https://www.php.net/manual/en/features.http-auth.php)
- [Understanding HTTP Basic Authentication](https://www.digitalocean.com/community/tutorials/understanding-http-basic-authentication)
- [PHP Auth Basic Code Example](https://www.w3schools.com/php/php_examples.asp)