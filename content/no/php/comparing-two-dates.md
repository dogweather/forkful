---
title:                "PHP: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Når vi jobber med PHP programmering, er det ofte vi står overfor utfordringen med å sammenligne to datoer. Dette kan være for å finne ut om en dato er før eller etter en annen, eller for å sjekke om to datoer faller innenfor samme tidsperiode. Uansett årsaken, er det viktig å kunne sammenligne datoer riktig for å oppnå ønsket funksjonalitet i koden vår.

## Hvordan du sammenligner to datoer i PHP

Sammenligning av datoer kan gjøres på ulike måter i PHP, avhengig av hva vi ønsker å oppnå. La oss se på noen eksempler på hvordan vi kan gjøre dette.

For å sammenligne om en dato er før eller etter en annen, kan vi bruke funksjonen `strtotime()` sammen med `if`-setninger. La oss si at vi vil sjekke om datoen `2021-01-20` er etter datoen `2021-01-15`. Vi kan skrive følgende kode:

```PHP
$first_date = strtotime('2021-01-20');
$second_date = strtotime('2021-01-15');

if ($first_date > $second_date) {
    echo "Første dato er etter den andre.";
} else {
    echo "Første dato er før den andre.";
}
```

Dette vil gi oss følgende utdatummer:

```
Første dato er etter den andre.
```

Vi kan også bruke funksjonen `DateTime` for å sammenligne datoer. La oss si at vi ønsker å finne ut om datoen `2021-02-05` faller innenfor en bestemt måned. Vi kan da bruke følgende kode:

```PHP
$date = DateTime::createFromFormat('Y-m-d', '2021-02-05');
$month = $date->format('m');

if ($month == '02') {
    echo "Datoen faller innenfor februar.";
} else {
    echo "Datoen faller ikke innenfor februar.";
}
```

Dette vil gi oss følgende utdatummer:

```
Datoen faller innenfor februar.
```

Det finnes også en rekke andre funksjoner og metoder for å sammenligne datoer i PHP, avhengig av hva du ønsker å oppnå. Det er viktig å lese dokumentasjonen nøye og velge den beste metoden for ditt spesifikke tilfelle.

## Dypdykk

For å virkelig mestre sammenligning av datoer i PHP, er det viktig å forstå hvordan datoen er representert i koden. Datoer i PHP er representert som antall sekunder siden 01.01.1970 kl. 00:00 GMT. Dette kalles en timestamp. Når vi bruker funksjonen `strtotime()`, blir datoen automatisk konvertert til en timestamp, og gjør det enklere å sammenligne.

Det er også viktig å være oppmerksom på at datoer kan variere basert på tidsoneinnstillinger og sommertid. Dette kan påvirke resultatet av sammenligningen din. Det kan derfor være lurt å bruke funksjoner som `date_default_timezone_set()` og `date_default_timezone_get()` for å sikre at datoene dine blir sammenlignet på riktig måte.

## Se også

- [PHP dokumentasjon for dato- og tidsfunksjoner](https://www.php.net/manual/en/ref.datetime.php)
- [W3Schools tutorial om å sammenligne datoer i PHP](https://www.w3schools.com/php/php_date.asp)
- [Phptherightway.com guide for å arbeide med datoer i PHP](https://phptherightway.com/#datetime)