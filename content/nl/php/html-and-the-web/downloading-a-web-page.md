---
title:                "Een webpagina downloaden"
aliases:
- /nl/php/downloading-a-web-page/
date:                  2024-01-28T21:59:28.287773-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het downloaden van een webpagina betekent het binnenhalen van de online inhoud, zodat je deze offline kunt gebruiken of analyseren. Programmeurs doen dit voor web scraping, gegevensanalyse of om programmatisch met webinhoud om te gaan.

## Hoe:

PHP maakt het downloaden van een webpagina vrij eenvoudig. Hier is een simpel voorbeeld met `file_get_contents()`:

```php
<?php
$url = "http://example.com";
$pageContent = file_get_contents($url);

if ($pageContent !== false) {
    echo "Pagina succesvol gedownload.\n";
    // Doe dingen met $pageContent
} else {
    echo "Mislukt om de pagina te downloaden.\n";
}
?>
```

En als je meer controle nodig hebt of HTTP-headers, cookies of POST-verzoeken wilt afhandelen, kun je gebruik maken van `cURL`:

```php
<?php
$url = "http://example.com";
$ch = curl_init($url);

curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$pageContent = curl_exec($ch);

if (curl_errno($ch)) {
    echo "Fout: " . curl_error($ch) . "\n";
} else {
    echo "Pagina succesvol gedownload.\n";
    // Doe dingen met $pageContent
}

curl_close($ch);
?>
```

Voorbeeld van output zou kunnen zijn:
```
Pagina succesvol gedownload.
```

## Diepgaande Uitleg

Webpagina's downloaden is een praktijk zo oud als het web zelf. Om aanvankelijk met webpagina's te interageren, zou je command-line tools zoals `wget` of `curl` gebruiken. Echter, naarmate PHP evolueerde, maakten functies deze taken binnen scripts mogelijk.

Laten we vergelijken:

- `file_get_contents()`: Eenvoudig voor simpele taken maar mist geavanceerde functies. Goed voor snelle grepen zonder gedoe.
- `cURL`: Het Zwitserse zakmes voor webverzoeken in PHP. Beheert complexe scenario's zoals authenticatie, cookies en het instellen van headers. Een beetje logger, maar daar wanneer je de extra spierkracht nodig hebt.

Achter de schermen stuurt `file_get_contents()` een standaard GET-verzoek. Dat betekent dat het zich gedraagt net als een browser wanneer je een URL intypt. Maar zonder HTTP-context (zoals headers), kunnen sommige pagina's niet de juiste inhoud teruggeven.

`cURL`, daarentegen, kan browsergedrag tot in de puntjes nabootsen. Dat is noodzakelijk voor de lastige pagina's die bepaalde headers of cookies verwachten.

Onthoud, sommige sites stellen het niet op prijs om gescrapt te worden. Respecteer altijd `robots.txt` en de gebruiksvoorwaarden.

## Zie Ook

- [PHP Handleiding over file_get_contents()](http://php.net/manual/en/function.file-get-contents.php)
- [PHP Handleiding over cURL](http://php.net/manual/en/book.curl.php)
- [robots.txt Specificaties](https://developers.google.com/search/docs/advanced/robots/robots_txt)
