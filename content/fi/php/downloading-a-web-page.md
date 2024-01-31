---
title:                "Verkkosivun lataaminen"
date:                  2024-01-20T17:44:46.068460-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Webbisivun lataaminen tarkoittaa sivun sisällön noutamista internetistä. Ohjelmoijat lataavat sivuja datan keräämiseen, API-vasteiden tarkistamiseen ja automaation toteuttamiseen.

## How to: (Kuinka tehdä:)
PHP:llä webbisivun lataaminen onnistuu `file_get_contents`-funktiolla tai cURL-kirjastolla. Tässä on esimerkki molemmista.

```PHP
<?php
// Esimerkki file_get_contents käytöstä
$sivunSisalto = file_get_contents('http://example.com');
echo $sivunSisalto;

// Esimerkki cURL:n käytöstä
$curl = curl_init('http://example.com');
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
$sivunSisaltoCurl = curl_exec($curl);
curl_close($curl);
echo $sivunSisaltoCurl;
?>
```

Sample output näyttäisi esimerkiksi näin:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
<body>
    ...
</body>
</html>
```

## Deep Dive (Sukellus syvyyksiin):
Historiallisesti PHP:ssä tiedon noutaminen etäpalvelimilta on kehittynyt yksinkertaisista funktioista monipuolisempiin kirjastoihin. `file_get_contents` on helppo tapa aloittaa, mutta sen ominaisuudet ovat rajoitetut: esimerkiksi HTTP-metodit tai headerit eivät ole konfiguroitavissa. cURL taas tarjoaa laajan valikoiman vaihtoehtoja pyyntöjen mukauttamiseen, mukaan lukien timeout-asetukset ja proxy-tuki.

cURL on tehokas väline monimutkaisempiin tehtäviin, mutta sen monimutkaisempi käyttöliittymä saattaa tuntua ylimitoitetulta yksinkertaisiin skenaarioihin verrattuna `file_get_contents`-funktioon. Viimeisissä PHP-versioissa tietoturva ja suorituskyky ovat olleet päähuolenaiheita, ja kehittäjät käyttävät usein kehyksiä tai paketteja, kuten Guzzle, helpottamaan HTTP-pyyntöjen hallintaa.

## See Also (Katso myös):
- PHP Manual `file_get_contents`: https://www.php.net/manual/en/function.file-get-contents.php
- PHP Manual cURL: https://www.php.net/manual/en/book.curl.php
- GuzzleHTTP GitHub page: https://github.com/guzzle/guzzle
