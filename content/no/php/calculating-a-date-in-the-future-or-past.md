---
title:                "PHP: Beregning av en dato i fremtiden eller fortiden"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#Hvorfor

Å beregne datoer i fremtiden eller fortiden er en viktig del av mange programmeringsoppgaver. Det kan være for å planlegge fremtidige hendelser, for å lage algoritmer eller for å finne feil i koden. Uansett grunn, kan det være nyttig å kunne utføre denne oppgaven raskt og enkelt i PHP.

#Slik gjør du det

For å beregne en dato i fremtiden eller fortiden i PHP, kan du bruke funksjonen "strtotime()". Denne funksjonen konverterer en tekstlig beskrivelse av en dato til en "Unix timestamp", som er en numerisk verdi som representerer antall sekunder som har gått siden 1. januar 1970. La oss se på noen eksempler:

```PHP
// Beregn dato 1 uke fra nå
$futureDate = strtotime("+1 week");
echo date("d/m/Y", $futureDate); // Output: 18/06/2021

// Beregn dato 3 måneder tilbake i tid
$pastDate = strtotime("-3 months");
echo date("d/m/Y", $pastDate); // Output: 15/03/2021

// Beregn dato 2 år og 2 uker fra nå
$futureDate = strtotime("+2 years +2 weeks");
echo date("d/m/Y", $futureDate); // Output: 08/07/2023
```

Som du kan se, kan du bruke forskjellige tidslinjer og deltaer for å få nøyaktig det resultatet du trenger. Du kan også kombinere flere parametere for å få mer presise datoer.

#Dypdykk

Hvis du vil lære mer om hvordan "strtotime()" fungerer og alle mulighetene den har, kan du se på PHPs offisielle dokumentasjon på [php.net](https://www.php.net/manual/en/function.strtotime.php). Her vil du finne en grundig beskrivelse av funksjonen og eksempler på forskjellige brukstilfeller.

Det er også viktig å vite at "strtotime()" har sine begrensninger når det gjelder å beregne datoer som ligger flere århundrer tilbake eller frem i tid. I slike tilfeller kan det være bedre å bruke "DateTime" klassen i PHP, som gir mer presise resultater og støtter et bredere utvalg av datoer.

#Se også

- [PHPs offisielle håndbok for "strtotime()"](https://www.php.net/manual/en/function.strtotime.php)
- [PHPs offisielle håndbok for "DateTime" klassen](https://www.php.net/manual/en/class.datetime.php)
- [Enkel dato og tidsbehandling i PHP](https://www.w3schools.com/php/php_date.asp)