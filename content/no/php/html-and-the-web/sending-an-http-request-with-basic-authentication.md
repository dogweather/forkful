---
date: 2024-01-20 18:02:02.333303-07:00
description: "Sending av en HTTP-foresp\xF8rsel med grunnleggende autentisering inneb\xE6\
  rer \xE5 inkludere brukernavn og passord for tilgangskontroll hos en server. Programmere\u2026"
lastmod: '2024-03-13T22:44:40.886177-06:00'
model: gpt-4-1106-preview
summary: "Sending av en HTTP-foresp\xF8rsel med grunnleggende autentisering inneb\xE6\
  rer \xE5 inkludere brukernavn og passord for tilgangskontroll hos en server."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

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
