---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, eller "Comma-Separated Values", er et filformat som brukes for lagring av data i en enkel tekstformat der hver linje er en datarad, og hvert felt er skilt med komma. Programmerere bruker CSV fordi det er enkelt, universelt og lett å importere til og eksportere fra databaser og regneark.

## How to:
Å jobbe med CSV-filer i PHP er rett fram. Her er kode for å lese og skrive.

### Lese CSV
```PHP
<?php
$filnavn = 'data.csv';
$handle = fopen($filnavn, 'r');
while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
    // behandle hver rad her
    print_r($data);
}
fclose($handle);
?>
```
Output kan se slik ut for en enkel rad:
```
Array
(
    [0] => Navn
    [1] => Alder
    [2] => By
)
```

### Skrive til CSV
```PHP
<?php
$liste = array(
    array('Navn', 'Alder', 'By'),
    array('Ola', '23', 'Oslo'),
    array('Kari', '35', 'Bergen')
);

$filnavn = 'ny_data.csv';
$handle = fopen($filnavn, 'w');

foreach ($liste as $rad) {
    fputcsv($handle, $rad);
}

fclose($handle);
?>
```

## Deep Dive
CSV har vært i bruk siden 1970-tallet og er fortsatt populært grunnet dets enkelhet. Alternativer til CSV inkluderer JSON og XML, som begge tillater mer komplekse strukturer. Når man implementerer CSV-håndtering i PHP, er det viktig å huske på tegnsett og feilhåndtering for å sikre riktig dataintegritet.

## See Also
- Mer om CSV-formatet: [tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- Sammenlikning av dataformater (inkludert CSV, JSON, XML): [w3schools.com](https://www.w3schools.com)
