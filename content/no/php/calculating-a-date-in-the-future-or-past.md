---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "PHP: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger vil du kanskje trenge å beregne en dato i fremtiden eller fortiden i PHP. Dette kan være nyttig for å lage en dynamisk kalender eller for å håndtere tidssensitive oppgaver.

## Hvordan
For å beregne en dato i PHP kan du bruke funksjonen `strtotime()`, som tar inn et datostreng og returnerer et dato-objekt.

```PHP
$date = strtotime("+1 week"); // Returnerer datoen 1 uke fra nå
echo date("d.m.Y", $date); // Skriver ut datoen i ønsket format (dd.mm.yyyy)
```

En annen måte å beregne datoer på er å bruke `DateTime`-klassen, som gir mer fleksibilitet og funksjoner for å håndtere datoer. Her er et eksempel på å beregne en dato 7 dager fra nå:

```PHP
$date = new DateTime();
$date->modify("+7 days"); // Legger til 7 dager til nåværende dato
echo $date->format("d.m.Y"); // Skriver ut datoen i ønsket format
```

For å beregne en dato i fortiden bruker du en negativ verdi. Du kan også beregne datoer basert på en annen dato, ved å tilpasse `strtotime()` eller bruke `add()`-funksjonen i `DateTime`-klassen.

## Deep Dive
Det er viktig å huske at PHP bruker serverens standardtidsone, og du bør alltid sette tidsone ved hjelp av `date_default_timezone_set()`-funksjonen for å unngå uventede resultater.

Når du bruker `strtotime()`, kan du også legge til eller trekke fra forskjellige tidsenheter som "days", "weeks" eller "months". Se PHPs dokumentasjon for en komplett liste.

I `DateTime`-klassen kan du bruke metoder som `add()` og `sub()` for å legge til eller trekke fra tidsenheter, og `diff()` for å beregne forskjellen mellom to datoer.

## Se Også
- [PHPs dokumentasjon for strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [PHPs dokumentasjon for DateTime-klassen](https://www.php.net/manual/en/class.datetime.php)
- [Tutorial: Working with Dates and Time in PHP](https://www.php.net/manual/en/tutorial.datetime.php)