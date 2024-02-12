---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
aliases:
- no/php/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:02.333303-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sending av en HTTP-forespørsel med grunnleggende autentisering innebærer å inkludere brukernavn og passord for tilgangskontroll hos en server. Programmere gjør dette for å sikre ressurser på nettet, og la kun autoriserte brukere få tilgang.

## Hvordan gjøre det:

```php
<?php
$url = 'https://eksempel.no/api/data';
$username = 'brukeren';
$password = 'hemmelighet';

$context = stream_context_create([
    'http' => [
        'header' => 'Authorization: Basic ' . base64_encode("$username:$password"),
        'method' => 'GET',
    ]
]);

$result = file_get_contents($url, false, $context);

if ($result !== FALSE) {
    echo "Suksess: " . $result;
} else {
    echo "Feil ved forespørsel.";
}
?>
```
Utskrift:
```
Suksess: { "svar": "Hei, autorisert bruker!" }
```

## Dypdykk

HTTP Basic Authentication har vært en del av HTTP-protokollen fra starten, enkel og grei for mindre sikkerhetskritiske tilfeller. Alternativer inkluderer OAuth, API-nøkler og JWT (JSON Web Tokens), som tilbyr bedre sikkerhet for mer komplekse systemer. Implementering av Basic Authentication krever forsiktighet; alltid bruke HTTPS for å beskytte legitimasjonen i transitt.

## Se også:

- MDN Web Docs om Basic Authentication: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- PHP Manual om `stream_context_create`: [https://www.php.net/manual/en/function.stream-context-create.php](https://www.php.net/manual/en/function.stream-context-create.php)
- Hvordan implementere sikrere autentiseringssystemer, som OAuth: [https://oauth.net/2/](https://oauth.net/2/)
