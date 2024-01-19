---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP-pyynnön lähettäminen perusautentikoinnilla PHP:llä

## Mikä & Miksi?
HTTP-pyynnön lähettäminen perusautentikoinnilla tarkoittaa tietyn API-avaimen lähettämistä palvelimelle autentikoimiseksi. Ohjelmoijat tekevät tämän tietojen suojaamiseksi ja sen varmistamiseksi, että vain oikeutetut käyttäjät voivat käyttää tiettyjä resursseja.

## Näin se tehdään:
Alla oleva PHP-koodi lähettää HTTP-pyynnön perusautentikoinnilla.

```PHP 
<?php
$context = stream_context_create(array(
    'http' => array(
        'header'  => "Authorization: Basic " . base64_encode("username:password")
    )
));
$response = file_get_contents("http://example.com", false, $context);
?>
```

Koodi luo ensin yhteyden tiedostovirtaan, johon lisätään HTTP-otsikko perusautentikointia varten. Sitten käytämme `file_get_contents`-funktiota lähettämään pyynnön.

## Syvempi sukellus:
Aikoinaan, PHP koodari ei pystynyt lähettämään HTTP-pyyntöjä perusautentikoinnin kanssa. Tämä oli mahdollista vain käyttämällä CURL-kirjastoa tai muita kolmannen osapuolen kirjastoja.

On myös muita tapoja autentikointiin kuten OAuth, token-autentikointi ja digest-autentikointi. Mutta perusautentikointi on kaikkein yksinkertaisin, ja se soveltuu yksinkertaisiin sovelluksiin tai nopeaan testaukseen.

PHP `file_get_contents`-funktion sisäinen käsittely antaa ohjelmoijan lähettää HTTP-pyyntöjä mukautetulla otsikolla. Tätä toiminnallisuutta voidaan käyttää moniin tarkoituksiin, kuten API-kyselyihin ja web-sivujen kaavintaan.

## Katso myös:
1. PHP dokumentointi HTTP-kontekstin luomisesta: [Linkki](https://www.php.net/manual/en/context.http.php)
2. Curl-kirjaston käyttö PHP:llä: [Linkki](https://www.php.net/manual/en/book.curl.php)
3. OAuth-autentikointi PHP:llä: [Linkki](https://oauth.net/code/php/)