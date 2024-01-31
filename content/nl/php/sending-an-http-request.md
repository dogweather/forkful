---
title:                "Een HTTP-verzoek verzenden"
date:                  2024-01-28T22:07:39.295246-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"

category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek versturen is het proces waarbij een programma om gegevens van een server vraagt. Programmeurs doen dit om te interageren met webservices, API's of om simpelweg de inhoud van een webpagina op te halen.

## Hoe:

PHP heeft een nette manier om HTTP-verzoeken te behandelen met de `cURL` bibliotheek. Maar de nieuwere methode is het gebruik van `file_get_contents` voor eenvoudigere GET-verzoeken, of de `stream_context_create` voor POST-verzoeken. Hier is een snelle blik op beide.

### GET-verzoek met file_get_contents():
```php
// De URL waar je een verzoek aan doet
$url = "http://example.com/api";

// Gebruik file_get_contents om een GET-verzoek uit te voeren
$response = file_get_contents($url);

// Dump uitvoer om te zien wat je hebt gekregen
var_dump($response);
```

### POST-verzoek met stream_context_create():
```php
// De URL waar je naar post
$url = "http://example.com/api";

// De gegevens die je verzendt
$data = http_build_query([
    'foo' => 'bar',
    'baz' => 'qux',
]);

// Stream context opties
$options = [
    'http' => [
        'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
        'method'  => 'POST',
        'content' => $data,
    ],
];

// Maak een streamcontext
$context  = stream_context_create($options);

// Voer het POST-verzoek uit en zet het antwoord in een variabele
$result = file_get_contents($url, false, $context);

// Zie wat je hebt ontvangen
var_dump($result);
```

## Diepgaande duik

Vroeger was `fsockopen()` de gangbare methode voor PHP HTTP-verzoeken. Het was omslachtig, maar het werkte. Toen kwam `cURL`, nog steeds krachtig en veel gebruikt, vooral voor complexe bewerkingen. Maar soms heb je geen kettingzaag nodig om een touwtje door te snijden. Daar blinken `file_get_contents()` en `stream_context_create()` in uit.

Een belangrijk punt over `file_get_contents()` is de eenvoud. Perfect voor simpele GET-verzoeken. Maar wat als je gegevens moet POSTen? Voer `stream_context_create()` in. Dit juweeltje laat je je HTTP-verzoeken fijn afstellen met headers, methoden en meer.

Onder de motorkap gebruiken `file_get_contents()` en `stream_context_create()` PHP's stream wrappers. Deze vervangen de laag-niveau socket operaties die door `fsockopen()` worden afgehandeld.

Een nadeel? Foutafhandeling kan lastiger zijn. Als iets misgaat, zijn deze functies minder vergevingsgezind dan `cURL`. Als je gedetailleerde responsinformatie nodig hebt of te maken hebt met complexe HTTP-taken, overweeg dan bij `cURL` te blijven.

## Zie Ook

- cURL's officiële PHP documentatie: [https://www.php.net/manual/en/book.curl.php](https://www.php.net/manual/en/book.curl.php)
- PHP streamcontexten: [https://www.php.net/manual/en/context.php](https://www.php.net/manual/en/context.php)
- HTTP-contextopties: [https://www.php.net/manual/en/context.http.php](https://www.php.net/manual/en/context.http.php)
