---
title:                "Sammenligning av to datoer"
html_title:           "PHP: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Å sammenligne to datoer kan være nyttig når man skal filtrere eller sortere gjennom store datasett, for eksempel i en database eller i et program. Dette kan også være viktig når man skal vise data på en organisert og forståelig måte for brukere.

## Hvordan
```PHP
$first_date = '2021-01-10';
$second_date = '2021-01-15';

// Bruk strtotime-funksjonen for å konvertere datoene til en tidsstamp
$first_timestamp = strtotime($first_date);
$second_timestamp = strtotime($second_date);

// Sammenligne datoene ved å trekke den første fra den andre
$diff = $second_timestamp - $first_timestamp;

// Resultatet blir i sekunder, så vi kan konvertere det til dager
$dager = floor($diff/(60*60*24));

echo 'Mellom ' . $first_date . ' og ' . $second_date . ' er det ' . $dager . ' dager.';
```

Dette kodeeksempelet viser hvordan man kan sammenligne to datoer og få antall dager mellom dem ved å bruke strtotime og enkel matematikk. Output vil være "Mellom 2021-01-10 og 2021-01-15 er det 5 dager."

```PHP
$first_date = '2021-01-20';
$second_date = '2021-01-10';

if ($first_date > $second_date) {
    echo $first_date . ' er senere enn ' . $second_date;
} else {
    echo $first_date . ' er før ' . $second_date;
}
```

I dette eksempelet bruker vi en if/else-setning for å sammenligne to datoer og få en utskrift av hvilken dato som er senere. Output vil være "2021-01-20 er senere enn 2021-01-10."

## Deep Dive
Det er mulig å sammenligne datoer på flere måter, for eksempel ved å bruke funksjoner som DateTime og DateInterval i PHP. Disse kan være nyttige når man trenger å håndtere mer komplekse datoer, som for eksempel med tidsone eller sommer- og vintertid.

Det er også viktig å være klar over at datoer kan være forskjellige i ulike deler av verden, på grunn av ulike tidszoner og datostandarder. Derfor er det viktig å være bevisst på hvilken datoformat man bruker og konvertere det til det ønskede formatet før man sammenligner.

## Se også
- [Php.net - strtotime](https://www.php.net/manual/en/function.strtotime.php)
- [Php.net - DateTime](https://www.php.net/manual/en/class.datetime.php)
- [Php.net - DateInterval](https://www.php.net/manual/en/class.dateinterval.php)