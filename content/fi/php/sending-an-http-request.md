---
title:                "PHP: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ohjelmoijat haluavat lähettää HTTP-pyyntöjä? HTTP-pyynnöt ovat olennainen osa verkkosovellusten ja -palveluiden toimintaa. Niiden avulla voimme lähettää tietoa verkkosivustoille ja noutaa tietoa takaisin. Esimerkiksi, jos haluamme hakea tietoja ulkoisesta APIsta tai tallentaa lomakkeen tiedot tietokantaan, niin HTTP-pyynnöt ovat välttämättömiä.

## Kuinka

PHP:n avulla on helppo lähettää HTTP-pyyntöjä ja käsittellä vastauksia. Alla on esimerkki, jossa lähetämme GET-pyynnön ulkoiselle APIlle ja tulostamme vastauksen.

````PHP
<?php

// Luodaan uusi HTTP-pyyntö
$request = new HttpRequest();
// Määritetään pyynnön URL
$request->setUrl('https://example.com/api');
// Asetetaan pyynnön HTTP-metodi
$request->setMethod(HTTP_METH_GET);
// Suoritetaan pyyntö ja tallennetaan vastaus muuttujaan
$response = $request->send();
// Tulostetaan vastaus ruudulle
echo $response->getBody();
````

Tässä esimerkissä käytämme `HttpRequest`-luokkaa, joka on osa PHP:n `pecl_http`-laajennusta. Pyyntöön voimme määrittää haluamamme URLin ja HTTP-metodin, kuten GET tai POST. Vastaus tallennetaan `HttpResponse`-luokan kautta ja voimme tulostaa esimerkiksi vastauksen koodin tai JSON-muodossa.

## Syvempi sukellus

HTTP-pyyntöjen lähettäminen on tärkeä taito jokaiselle web-ohjelmoijalle. Lisäksi PHP:n `pecl_http`-laajennus tarjoaa monia muita hyödyllisiä ominaisuuksia, kuten käsittelyvirheiden hallinnan ja rajapintapyyntöjen toteuttamisen. Kannattaa tutustua tarkemmin laajennuksen dokumentaatioon ja kokeilla erilaisia HTTP-pyyntöjä eri tilanteissa.

## Katso myös

- [PHP:n pecl_http-laajennuksen dokumentaatio](https://www.php.net/manual/en/book.http.php)
- [HTTP-pyyntöjen hallinnan perusteet](https://developer.mozilla.org/fi/docs/Web/HTTP/Overview)
- [RESTful API -palveluiden toteuttaminen PHP:lla](https://www.sitepoint.com/restful-api-php-slim-framework3/)