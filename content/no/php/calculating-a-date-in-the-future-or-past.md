---
title:    "PHP: Beregning av en dato i fremtiden eller fortiden"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å ville beregne en dato i fremtiden eller fortiden. Kanskje du trenger å planlegge en hendelse eller ønsker å lage en alderskalkulator. Uansett årsak, er PHP et flott språk å bruke for å utføre slike beregninger.

## Hvordan gjøre det

Beregning av en dato i fremtiden eller fortiden innebærer å legge til eller trekke fra et visst antall dager til en eksisterende dato. Det kan gjøres ved bruk av PHPs innebygde funksjoner `strtotime()` og `date()`.

Først må du bestemme deg for hvilken dato du vil utføre beregningen på, og deretter bestemme hvor mange dager du vil legge til eller trekke fra. For eksempel, hvis du ønsker å beregne datoen 2 måneder og 10 dager fra nå, vil vi bruke `date()` til å først hente den nåværende datoen, og deretter bruke `strtotime()` for å legge til 2 måneder og 10 dager.

```PHP
$todays_date = date("d-m-Y"); // henter nåværende dato
$future_date = date("d-m-Y", strtotime("+2 months +10 days")); // legger til 2 måneder og 10 dager
echo "Datoen 2 måneder og 10 dager fra nå vil være: $future_date";
```

Dette vil gi oss følgende utdata i nettleseren:

*Datoen 2 måneder og 10 dager fra nå vil være: 02-08-2020*

Vi kan også beregne en dato i fortiden ved å bruke `strtotime()` med en negativ verdi. For eksempel, hvis vi ønsker å finne ut datoen 3 uker tilbake i tid, vil vi bruke følgende kode:

```PHP
$past_date = date("d-m-Y", strtotime("-3 weeks")); // trekker fra 3 uker
echo "Datoen 3 uker tilbake i tid var: $past_date";
```

Som et resultat vil vi få følgende utdata:

*Datoen 3 uker tilbake i tid var: 03-04-2020*

## Dypdykk

Å utføre beregninger av datoer i fremtiden eller fortiden kan virke enkelt, men det er viktig å være klar over noen ting før du implementerer det i ditt eget prosjekt. For det første kan `strtotime()` bare beregne datoen fra og med 1. januar 1970. For det andre, må du være forsiktig med datoen som returneres hvis du bruker `date()` med forskjellige formater.

Det er også verdt å merke seg at du kan bruke andre enheter enn dager i `strtotime()`, for eksempel måneder eller år. Du kan også kombinere enheter, for eksempel "+5 months -3 days".

## Se Også

- [PHP `strtotime()` funksjon](https://www.php.net/manual/en/function.strtotime.php)
- [PHP `date()` funksjon](https://www.php.net/manual/en/function.date.php)