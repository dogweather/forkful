---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer i programmering refererer til å analysere hvilken dato som kommer først i tidslinjen. Dette er viktig for å håndtere tidsavhengige prosesser som planlegging, påminnelser, eller tidslinjefunksjoner i et program.

## Hvordan gjør man det?

La oss bare kjøre rett inn i det. Følgende er en grunnleggende måte å sammenligne to datoer i PHP:

```PHP
<?php
$dato1 = new DateTime("2022-01-01");
$dato2 = new DateTime("2022-02-01");

if ($dato1 < $dato2) {
  echo "Dato1 er mindre enn Dato2";
} else {
  echo "Dato1 er større enn Dato2";
}
?>
```

Utdata vil være: "Dato1 er mindre enn Dato2"

## Dykk Dypt

Historisk sett var det mange måter å sammenligne datoer i tidligere PHP-versjoner, men med DateTime-klasser har det blitt enklere og mer intuisivt.

Alternativer til DateTime-klassen<br>
Du kan også bruke funksjoner som `strtotime()` for å konvertere datoer til tidsstempel og sammenligne dem.

```PHP
<?php
$dato1 = strtotime("2022-01-01");
$dato2 = strtotime("2022-02-01");

if ($dato1 < $dato2) {
  echo "Dato1 er mindre enn Dato2";
} else {
  echo "Dato1 er større enn Dato2";
}
?>
```

Implementeringsdetaljer<br>
Merk den faktiske sammenligningen er `<` eller `>` operatørene i koden. DateTime-objektene kan sammenlignes på denne måten fordi de implementerer "Magic Methods" __toString og __get.

## Se også

For mer informasjon, sjekk ut disse kildene:

1. PHPs offisielle dokumentasjon for [DateTime-klassen](https://www.php.net/manual/en/class.datetime.php)
2. [Sammenligning av datoer](https://www.php.net/manual/en/datetime.diff.php) med DateTime::diff
3. En mer detaljert forklaring av [PHP Magic Methods](https://www.php.net/manual/en/language.oop5.magic.php)