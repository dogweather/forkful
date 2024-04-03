---
date: 2024-01-20 18:00:11.703472-07:00
description: "PHP:ss\xE4 HTTP-pyynt\xF6 l\xE4hett\xE4\xE4 tietoja palvelimelle tai\
  \ hakee niit\xE4 sielt\xE4. Ohjelmoijat tekev\xE4t t\xE4m\xE4n, jotta voivat siirt\xE4\
  \xE4 dataa, hakea resursseja tai\u2026"
lastmod: '2024-03-13T22:44:56.652571-06:00'
model: gpt-4-1106-preview
summary: "PHP:ss\xE4 HTTP-pyynt\xF6 l\xE4hett\xE4\xE4 tietoja palvelimelle tai hakee\
  \ niit\xE4 sielt\xE4."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## What & Why? (Mitä & Miksi?)
PHP:ssä HTTP-pyyntö lähettää tietoja palvelimelle tai hakee niitä sieltä. Ohjelmoijat tekevät tämän, jotta voivat siirtää dataa, hakea resursseja tai kommunikoida muiden palveluiden kanssa.

## How to: (Miten tehdään:)
PHP:ssä HTTP-pyyntöjen tekeminen voi olla yksinkertaista. Tässä esimerkkejä `file_get_contents`- ja `cURL`-funktioilla:

```PHP
// Perus GET-pyyntö file_get_contents-funktiolla
$response = file_get_contents('https://api.example.com/data');
echo $response;

// cURL:lla tehokkaampi vaihtoehto
$ch = curl_init('https://api.example.com/data');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($ch);
curl_close($ch);
echo $response;
```

Molemmat palauttavat palvelimelta saadun vastauksen.

## Deep Dive (Sukellus syvemmälle)
Alun perin PHP:ssä HTTP-pyyntöjä lähetettiin suorilla socket-funktioilla, mutta se oli monimutkaista. `file_get_contents` ja `fopen` -funktiot tarjosivat yksinkertaistuksen, mahdollistaen nopeamman tiedonhaun.

`cURL` lib on kehitettynä lisäfunktioilla varustettu työkalu, joka mahdollistaa monipuolisemmat HTTP-pyynnöt, kuten POST, HEAD, PUT ja DELETE, sekä lisää ohjausta yhteydenhallintaan (esim. timeout, headerit).

Vaihtoehtoisia kirjastoja, kuten Guzzle ja Requests for PHP, on myös olemassa, jotka tarjoavat objektilähtöisen lähestymistavan ja helppokäyttöisen API:n.

## See Also (Katso myös)
- PHP.net cURL: https://www.php.net/manual/en/book.curl.php
- PHP.net HTTP context options: https://www.php.net/manual/en/context.http.php
- GuzzleHTTP documentation: http://docs.guzzlephp.org/en/stable/
- Requests for PHP: http://requests.ryanmccue.info/
