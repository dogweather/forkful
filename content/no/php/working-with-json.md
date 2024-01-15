---
title:                "Å jobbe med json"
html_title:           "PHP: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

JSON, eller JavaScript Object Notation, er et populært datautvekslingsformat for å lagre og overføre strukturerte data mellom nettapplikasjoner. Det er enklere å lese og skrive enn XML og er også støttet av de fleste programmeringsspråk. Derfor er det en viktig ferdighet for utviklere å kunne håndtere JSON-data i sine applikasjoner.

## Hvordan gjøre det

For å jobbe med JSON i PHP, må du først hente dataene fra en ekstern kilde eller opprette et JSON-objekt i selve koden din. Dette kan gjøres ved å bruke funksjonen `file_get_contents()` for å hente data fra en URL eller ved å opprette et JSON-objekt ved å bruke funksjonen `json_encode()`.

```PHP
// Hente data fra en URL
$json_data = file_get_contents('https://example.com/data.json');

// Opprette et JSON-objekt
$data = array(
  'navn' => 'John Doe',
  'alder' => 30,
  'hobbyer' => array('musikk', 'film', 'reise')
);

$json_data = json_encode($data);
```

Når du har dataene i et JSON-objekt, kan du enkelt hente ut dem ved å bruke funksjonen `json_decode()`. Det er viktig å huske at JSON-data er  lagret som strenger, så du må dekode det for å få tilgang til de faktiske dataene.

```PHP
// Hente ut navnet fra JSON-data
$navn = $json_data->navn;

// Hente ut hobbyene fra JSON-data
$hobbyer = $json_data->hobbyer;
print_r($hobbyer); // Array ( [0] => musikk [1] => film [2] => reise )
```

## Dypdykk

JSON-data er lagret som en streng i UTF-8-format, som kan håndteres av PHP uten problemer. Men hvis JSON-innholdet inneholder spesielle tegn, må du sørge for å konvertere dem tilbake til UTF-8-format ved å bruke funksjonen `utf8_encode()`.

En annen viktig ting å merke seg er at JSON-data kan ha ulike strukturer, som et array eller et objekt. Det er derfor viktig å sjekke JSON-strukturen før du prøver å hente ut dataene.

```PHP
// Sjekk om JSON-strukturen er et array
if (is_array($json_data)) {
  foreach ($json_data as $item) {
    echo $item->navn;
  }
}

// Sjekk om JSON-strukturen er et objekt
if (is_object($json_data)) {
  echo $json_data->navn;
}
```

## Se også

- [PHP manual: JSON Functions](https://www.php.net/manual/en/book.json.php)
- [Understanding JSON in PHP - Tutsplus](https://code.tutsplus.com/tutorials/understanding-json-in-php--cms-32457)
- [JSON Formatter](https://jsonformatter.org/)