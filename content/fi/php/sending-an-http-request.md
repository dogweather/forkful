---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen on prosessi, jossa tietoja lähetetään verkkosivulle tai sovellukseen. Ohjelmoijat tekevät sen esimerkiksi hakeakseen dataa, lähettääkseen dataa tai tehdäkseen päivityksen jo olemassa olevaan dataan.

## Näin teet:

```PHP
<?php
// Alustetaan cURL
$ch = curl_init();

// Asetetaan URL ja muut vaihtoehdot
curl_setopt($ch, CURLOPT_URL, "http://esimerkki.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_HEADER, 0);

// Pyydetään URL ja tallennetaan se muuttujaan $output
$output = curl_exec($ch);

// Suljetaan cURL-istunto
curl_close($ch);

// Tulostetaan saatu tulos
echo $output;
?>
```

Esimerkkikoodimme lähettää GET-pyynnön "http://esimerkki.com"-osoitteeseen ja tulostaa saadun vastauksen. 

## Syvempi tieto:

HTTP-pyyntöjen lähettäminen on ollut olennainen osa web-sovelluksia aina WWW:n alkuaikojen PERL-skripteistä nykyisiin JavaScript-yksisivusovelluksiin. PHP:ssa on monta tapaa tehdä HTTP-pyynnön lähettäminen, kuten `file_get_contents()`, `fsockopen()`, ja `stream_context_create()`. 

Näistä kaikista cURL on kuitenkin tehokkain ja monipuolisin. cURL yleistyi sen tehokkaan suorituskyvyn ja tuekkaan kyvynsä käsitellä eri protokollia ansiosta, jota muut PHP:n sisäänrakennetut funktiot eivät kykene. 

Huomaa kuitenkin, että cURL tulee asentaa ja ottaa käyttöön erikseen PHP:ssa, joka saattaa olla hankalaa joissakin ympäristöissä.

## Katso myös:

- PHP:n virallinen cURL-dokumentaatio (https://www.php.net/manual/en/book.curl.php)
- PHP: The Right Way - suositukset HTTP-pyynnön lähettämisestä (https://phptherightway.com/)
- Stack Overflow keskusteluja HTTP-pyynnöistä PHP:ssa (https://stackoverflow.com/questions/tagged/php+http-request)