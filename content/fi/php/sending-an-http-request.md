---
title:                "Http-pyynnön lähettäminen"
html_title:           "PHP: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lähettäminen HTTP-pyyntö on tapa, jolla ohjelmoijat voivat lähettää pyyntöjä muille verkkosivustoille HTTP-protokollan avulla. Tämä on hyödyllistä, koska se mahdollistaa tiedon hakemisen ja jakamisen useiden eri sivustojen välillä.

## Kuinka:
Esimerkiksi, voit käyttää PHP:ta lähettämään GET-pyynnön osoitteeseen "https://www.esimerkkisivusto.fi" ja tulostaa vastauksen tietokantaan käyttämällä seuraavaa koodia:
```PHP
$pyynto = file_get_contents("https://www.esimerkkisivusto.fi");
echo $pyynto;
```

Tulosteena saattaa olla sivuston HTML-koodi tai muu tieto, joka on saatavilla pyynnön antamasta sivustosta.

Voit myös lähettää POST-pyynnön lisäämällä haluamasi tiedot data-muuttujaan ja käyttämällä "CURLOPT_POST" asetuksena. Esimerkiksi:
```PHP
$data = array("nimi" => "Matti", "ika" => "30");
$pyynto = curl_init("https://www.esimerkkisivusto.fi");
curl_setopt($request, CURLOPT_POST, true);
curl_setopt($request, CURLOPT_POSTFIELDS, $data);
curl_exec($pyynto);
```

## Syvemmälle:
HTTP-pyyntöjen lähettäminen on tapa, jolla eri verkkosivustot kommunikoivat keskenään ja mahdollistavat tietojen jakamisen. Tätä tekniikkaa on käytetty jo vuosikymmeniä ja se on olennainen osa verkkokehitystä.

On myös muita tapoja lähettää HTTP-pyyntöjä, kuten käyttämällä JavaScriptia tai muita ohjelmointikieliä. Lisäksi on olemassa erilaisia kirjastoja ja työkaluja, jotka voivat auttaa lähettämään ja vastaanottamaan HTTP-pyyntöjä helposti.

## Katso myös:
- [PHP:n viralliset dokumentaatiot HTTP-pyyntöjen lähettämisestä](https://www.php.net/manual/en/function.file-get-contents.php)
- [Guide to HTTP Requests in JavaScript](https://www.javascripture.com/XMLHttpRequest)
- [HTTP Requests in Other Programming Languages](https://www.freecodecamp.org/news/make-a-http-request-with-python/)
- [CURL - Command Line Tool for Sending HTTP Requests](https://curl.haxx.se/)