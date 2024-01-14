---
title:                "PHP: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange situasjoner der man kan ha behov for å beregne en dato i fremtiden eller fortiden i et PHP-program. Dette kan være for å lage dynamiske tidsstempel eller for å håndtere tidsbaserte funksjoner som for eksempel abonnementsforlengelser. Uansett hva grunnen er, er det viktig å ha en god forståelse for hvordan man kan utføre disse beregningene i PHP.

## Hvordan gjøre det

Det første steget er å definere en variabel med dagens dato, som vi kan bruke som utgangspunkt for beregningene våre. Dette kan gjøres ved å bruke funksjonen "date" i PHP og angi formatet som ønskes. For eksempel:

```PHP
$dagens_dato = date("Y-m-d");
```

Deretter kan vi bruke funksjonen "strtotime" til å legge til eller trekke fra et antall dager fra denne datoen. Her er et eksempel som legger til 7 dager:

```PHP
$fremtidig_dato = strtotime($dagens_dato."+7 days");
```

For å få datoen i ønsket format bruker vi igjen funksjonen "date" og angir formatet som ønskes. I dette tilfellet vil vi ha datoen i formatet "dag/måned/år":

```PHP
$fremtidig_dato = date("d/m/Y", $fremtidig_dato);
```

Dette vil gi oss datoen 7 dager frem i tid fra dagens dato.

## Dypdykk

PHP har mange nyttige funksjoner for å håndtere datoer, men det kan være lurt å være oppmerksom på noen ting når man bruker dem. For eksempel vil funksjonen "strtotime" bruke dagens dato som utgangspunkt hvis datoen ikke er spesifisert, noe som kan føre til feil i beregningene hvis datoen ikke er korrekt definert. Det kan også være lurt å sjekke om datoen man ender opp med er en gyldig dato, for å unngå feilmeldinger.

## Se også

- [date funksjonen i PHP](https://www.php.net/manual/en/function.date.php)
- [strtotime funksjonen i PHP](https://www.php.net/manual/en/function.strtotime.php)