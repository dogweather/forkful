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

## Hva & Hvorfor?

Beregning av en dato i fremtiden eller fortiden er en prosess hvor du tilføyer eller trekker tid fra en gitt dato. Programmere gjør dette ofte for å håndtere begivenheter som abonnementsfornyelser, oppgavevarslinger, osv.

## Hvordan gjør man det:

PHP funksjonen `strtotime` kombinert med `date` kan brukes til å manipulere datoer. Her er et enkelt eksempel for hvordan du kan legge til 5 dager til dagens dato:

```PHP
$dato_idag = date('m-d-Y');
$dato_fremtid = date('m-d-Y', strtotime($dato_idag . ' + 5 days'));
echo "Dagens dato: " . $dato_idag;
echo "\nFremtid dato: " . $dato_fremtid;
```
Output vil bli:
```PHP
Dagens dato: 12-01-2022
Fremtid dato: 12-06-2022
```
## Dypdykk:

`strtotime` er en mektig funksjon introdusert i PHP 4, og har vært videreutviklet siden. Til tross for at det er fantastiske biblioteker som Carbon som kan brukes for mer komplekse dato- og tidsoperasjoner, er det for de fleste formål mer enn nok å bruke innebygde PHP funksjoner.

Bruken av `strtotime` er ofte å foretrekke fremfor tidsstempel manipuleringer på grunn av dens håndtering av sommertid og skuddår. I tillegg, ved bruk av `strtotime`, kan du angi tidsperioden ved å bruke naturlig språk (for eksempel '+1 day'), som gjør koden mer lesbar.

## Se Også:

1. PHP manual på [strtotime](https://www.php.net/manual/en/function.strtotime)
2. [Date/Time PHP Extensions](https://www.php.net/manual/en/refs.calendar.php)
3. [Carbon biblioteket](https://carbon.nesbot.com/docs/) for komplekse dato- og tidsoperasjoner