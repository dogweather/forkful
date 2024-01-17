---
title:                "Perusvahvistuksella http-pyyntöjen lähettäminen"
html_title:           "PHP: Perusvahvistuksella http-pyyntöjen lähettäminen"
simple_title:         "Perusvahvistuksella http-pyyntöjen lähettäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

### Mitä ja miksi?

Kun ohjelmoijat lähettävät HTTP-pyynnön perusautentikaation kanssa, he käyttävät salasanaa ja käyttäjänimeä lähettääkseen tietoja turvallisesti verkossa. Tämä on yleinen tapa tunnistaa käyttäjä tai sovellus palvelimelle.

### Miten:

Käyttäen PHP:ta, voit lähettää HTTP-pyynnön perusautentikaation kanssa yksinkertaisella koodilla. Alla on esimerkki, jossa lähetetään JSON-dataa palvelimelle käyttäjänimen ja salasanan avulla:

```PHP
<?php

$curl = curl_init();

curl_setopt_array($curl, array(
  CURLOPT_URL => "http://example.com/api/data",
  CURLOPT_RETURNTRANSFER => true,
  CURLOPT_ENCODING => "",
  CURLOPT_MAXREDIRS => 10,
  CURLOPT_TIMEOUT => 0,
  CURLOPT_FOLLOWLOCATION => true,
  CURLOPT_HTTP_VERSION => CURL_HTTP_VERSION_1_1,
  CURLOPT_CUSTOMREQUEST => "GET",
  CURLOPT_HTTPHEADER => array(
    "Authorization: Basic " . base64_encode("username:password"),
    "Content-Type: application/json"
  ),
));

$response = curl_exec($curl);

curl_close($curl);
echo $response;

?>
```

Tämä koodi lähettää GET-pyynnön osoitteeseen "http://example.com/api/data" ja lähettää samalla käyttäjänimen ja salasanan otsikkona käyttäen perusautentikaatiota. Palvelin vastaa sitten pyyntöön ja tietoja voi käsitellä edelleen PHP-koodissa.

### Syväsukellus:

Perusautentikaatio otettiin käyttöön HTTP-protokollaan jo vuonna 1999 ja sitä käytetään edelleen laajalti. Se on yksi yksinkertaisimmista tavoista tunnistaa käyttäjä tai sovellus palvelimella ja siksi suosittu valinta monissa ohjelmistoprojekteissa. On myös muita tapoja tunnistaa käyttäjä, kuten käyttäjätunnus/salasana-kirjautuminen tai OAuth-autentikaatio.

Jos haluat lähettää HTTP-pyynnön ilman perusautentikaatiota, voit käyttää esimerkiksi cURL-kirjastoa tai PHP:n sisäistä file_get_contents-funktiota. Nämä tavat eivät kuitenkaan ole yhtä turvallisia kuin perusautentikaatio.

Perusautentikaation käyttö PHP:ssa vaatii hieman enemmän koodia, mutta on silti melko yksinkertaista. Voit myös tarkistaa palvelimen vastauksen HTTP-koodin avulla ja käsitellä mahdolliset virheet koodissa.

### Katso myös:

Voit lukea lisää perusautentikaation käytöstä PHP:ssa PHP:n virallisesta dokumentaatiosta: [PHP Base Authentication](https://www.php.net/manual/en/features.http-auth.php). Voit myös tutustua erilaisiin tapoihin lähettää HTTP-pyyntöjä PHP:ssa [PHP HTTP Request options](https://www.php.net/manual/en/function.http-request.php).