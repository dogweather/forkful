---
title:                "Lähettämällä http-pyyntö perusautentikoinnilla"
html_title:           "PHP: Lähettämällä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettämällä http-pyyntö perusautentikoinnilla"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyynnön lähettäminen perusautentikoinnilla mahdollistaa käyttäjätunnuksen ja salasanan välittämisen palvelimelle suojatun yhteyden yli. Tämä on hyödyllistä, kun halutaan varmistaa, että vain oikeutetut käyttäjät pääsevät pääsyyn tietoihin tai toimintoihin.

## Kuinka

```PHP
<?php

// Asetetaan käyttäjätunnus ja salasana
$username = "käyttäjä";
$password = "salasana";

// Luodaan uusi HTTP-pyyntö GET-metodilla ja määritetään kohdesivu
$request = curl_init("https://www.example.com");

// Määritetään autentikointitiedot
curl_setopt($request, CURLOPT_USERPWD, $username . ":" . $password);

// Suoritetaan pyyntö ja tallennetaan vastauksen tiedot muuttujaan
$response = curl_exec($request);

// Suljetaan HTTP-pyyntö
curl_close($request);

// Tulostetaan vastauksen tiedot
echo $response;
```

Tässä esimerkissä käytetään PHP:n curl-kirjastoa lähettämään HTTP-pyyntö perusautentikoinnilla. Ensin määritetään käyttäjätunnus ja salasana muuttujiin. Tämän jälkeen luodaan uusi HTTP-pyyntö curl_init-funktiolla ja asetetaan pyynnön kohteeksi haluttu sivu. Lopuksi määritetään autentikointitiedot curl_setopt-funktiolla ja suoritetaan pyyntö curl_exec-funktiolla. Vastauksen tiedot tallennetaan muuttujaan ja sieltä ne voi tulostaa haluamallaan tavalla.

## Syvemmälle

Perusautentikointi on yksi vanhimmista ja yksinkertaisimmista autentikointimenetelmistä, joka käyttää base64-koodausta käyttäjätunnuksen ja salasanan välittämiseen. Koska tiedot ovat kuitenkin vain koodattuja eikä salattuja, sitä ei tulisi käyttää luottamuksellisten tietojen lähettämiseen. Lisäksi on tärkeää muistaa, että HTTP-pyyntöjen lähettäminen perusautentikoinnilla ei tarjoa suojaa palvelimen ja asiakkaan väliselle viestinnälle, joten suojattu yhteys (HTTPS) tulisi aina käyttää yhteyden varmistamiseen.

## Katso myös

- [PHP:n curl-kirjaston dokumentaatio](https://www.php.net/manual/en/book.curl.php)
- [Selitys HTTP-perusautentikoinnista](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)