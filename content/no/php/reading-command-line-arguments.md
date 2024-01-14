---
title:    "PHP: Lesing av kommandolinje-argumenter"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Mange veteran-programmerere er vant til å bruke grafiske brukergrensesnitt for å samhandle med sine programmer. Mens dette er en praktisk måte å jobbe på, kan det også være nyttig å lære kommando linje-argumenter som et alternativ. Det er mange fordeler ved å kunne lese og bruke kommando linje-argumenter, inkludert bedre kontroll over programmet og mer effektiv testing. I denne bloggposten skal vi gå gjennom hvordan man kan lese kommando linje-argumenter i PHP.

## Hvordan gjøre det

For å lese kommando linje-argumenter i PHP, kan man bruke det innebygde $_SERVER-variabelen og funksjonen getopt(). La oss se på et enkelt eksempel:

```PHP
<?php

// Les inn kommando linje-argumenter
$options = getopt("u:p:");

// Sjekk om argumentene er satt
if(isset($options['u']) && isset($options['p'])){
    // Bruk argumentene i koden din
    $username = $options['u'];
    $password = $options['p'];
    echo "Velkommen, $username!";
}else{
    echo "Brukernavn og passord må angis";
}

?>
```

I kodeeksempelet ovenfor bruker vi getopt() til å lese de to argumentene "-u" og "-p". Disse representerer brukernavn og passord og kan brukes i koden til å gjøre relevant handling. Hvis argumentene ikke er angitt, vil en melding bli vist for å minne brukeren om å legge dem til.

## Deep Dive

For å forstå mer om kommando linje-argumenter, kan det være nyttig å vite om viktige begrep som "flags" og "options". Flags er som brytere som indikerer at en bestemt handling skal utføres, for eksempel "-v" for å vise versjonsnummeret til programmet. Options er verdier som brukes til å angi spesifikke verdier, for eksempel "-u" for å angi brukernavnet. I koden ovenfor brukte vi både en flag og en option.

I tillegg til denne grunnleggende funksjonaliteten, er det også mulig å bruke flere kommando linje-argumenter og håndtere dem på ulike måter, for eksempel å samle dem inn i en array og bruke en løkke for å behandle dem. Det er også mulig å bruke regulære uttrykk for å validere argumentene som blir angitt.

## Se også

Lær mer om å jobbe med kommando linje-argumenter i PHP ved å sjekke ut disse ressursene:

- [Kommando linje-behandling i PHP](https://www.php.net/manual/en/features.commandline.php)
- [Brukerkommentarer om getopt()](https://www.php.net/manual/en/function.getopt.php#115139)
- [Arbeide med kommando linje-argumenter i PHP-enhetstester](https://blog.jetbrains.com/phpstorm/2019/06/unit-testing-cli-applications-with-phpstorm-and-phpunit/)

Lykke til med å implementere kommando linje-argumenter i dine PHP-prosjekter!