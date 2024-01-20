---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering er prosessen å overføre data over internett ved hjelp av Hypertext Transfer Protocol (HTTP) med en bruker-ID og passord. Programmerere gjør dette for å sikre sikker datatilgang og å beskytte mot uautorisert bruk.

## Hvordan:

Installer først `curl`-biblioteket:

```PHP
<?php
$ sudo apt-get install php-curl
?>
```
Her er et enkelt eksempel på å sende en GET-forespørsel med basic authentication:

```PHP
<?php
$curl = curl_init();

curl_setopt_array($curl, [
  CURLOPT_URL => "http://website.com",
  CURLOPT_RETURNTRANSFER => true,
  CURLOPT_USERPWD => 'user:password',
  CURLOPT_HTTPAUTH => CURLAUTH_BASIC,
]);

$response = curl_exec($curl);

if (curl_errno($curl)) {
  echo 'Error:' . curl_error($curl);
}
curl_close($curl);
?>
```

Og dette er output av koden:

```PHP
<?php
Output: 'Requested page HTML here...'
?>
```

## Dybdeplunge:

Mens HTTP Basic Authentication har en lang historie som begynner med sine røtter i tidlig internett, har det fått kritikk for sin manglende sikkerhetsfunksjoner sammenlignet med alternativer som OAuth.

Alternativene inkluderer OAuth og Digest Authentication. OAuth er en åpen standard som gir klienter en "sikker delegert tilgang" til serverressurser på vegne av en ressurseier. Digest Authentication er en metode for å bruke en hemmelig nøkkel for å autentisere en bruker, noe som gir mer sikkerhet enn Basic Authentication.

Når det gjelder gjennomføring av detaljer, er brukernavn og passord kodet med base64 og inkludert i `Authorization` header i HTTP-forespørselen.

## Se Også:

1. [PHP: HTTP authentication with PHP - Manual](https://www.php.net/manual/en/features.http-auth.php)
3. [HTTP Authentication - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)