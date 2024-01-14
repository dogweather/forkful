---
title:                "PHP: Lähettämällä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettämällä http-pyyntö perusautentikoinnilla"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen perusautentikoinnilla on tärkeää verkkosovellusten turvallisuuden kannalta. Tällä tavalla varmistetaan, että vain oikeutetut käyttäjät voivat käyttää ja hallita tiettyä resurssia.

## Miten

```PHP
// Määritetään URL-osoite
$url = 'https://esimerkkisivu.com';

// Luodaan autentikaatiotiedot
$username = 'käyttäjänimi';
$password = 'salasana';

// Luodaan HTTP-pyyntö perusautentikaatiolla
$ch = curl_init($url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_USERPWD, $username . ':' . $password);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
$output = curl_exec($ch);
curl_close($ch);

// Tulostetaan vastaus
echo $output;
```

**Tulos:**

```
Hello World!
```

## Syvemmälle

Perusautentikointi toimii lähettämällä HTTP-pyyntöjä otsikolla "Authorization", joka sisältää käyttäjänimen ja salasanan Base64-koodattuna. Tämä koodaus ei kuitenkaan ole täysin turvallinen, joten suositellaan käyttämään TLS-salausta yhteyden luomiseen ennen autentikaation lähettämistä.

## Katso myös

- [PHP cURL-dokumentaatio](https://www.php.net/manual/en/function.curl-setopt.php)
- [Base64-koodaus](https://en.wikipedia.org/wiki/Base64)
- [TLS-salaus](https://www.instantssl.com/ssl-certificate-products/https.html)