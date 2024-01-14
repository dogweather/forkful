---
title:                "PHP: Hente den nåværende datoen"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor få nåværende dato i PHP?

Å få nåværende dato i et programmeringsspråk som PHP kan være svært nyttig for å holde oversikt over tidsbegrensede funksjoner eller prosesser. Dette kan inkludere datofiltre for databaser eller å vise den nåværende datoen på et nettsted. Det kan også brukes til å vise dynamiske datoer for arrangementer eller tidsfrister. Uansett hva årsaken er, er det viktig å vite hvordan man kan få tak i den nåværende datoen i PHP.

## Slik gjør du det

For å få tak i nåværende dato i PHP er det flere måter å gjøre det på. Den enkleste måten er ved å bruke innebygde funksjoner som "date()" eller "time()". Disse funksjonene returnerer nåværende dato og tid i en bestemt format, avhengig av hvilke parametere som er gitt.

```PHP
$date = date("d-m-Y"); //Dette vil returnere nåværende dato på formatet DD-MM-YYYY
$time = time(); //Dette vil returnere nåværende tid i sekunder siden 1. januar 1970
```

En annen måte å få tak i nåværende dato på er ved å bruke "DateTime" klassen i PHP. Denne klassen gir mer fleksibilitet når det gjelder å manipulere datoer og tilpasse formatet.

```PHP
$date = new DateTime(); //Oppretter et nytt DateTime objekt med nåværende dato og tid
echo $date->format('d-m-Y'); //Dette vil returnere nåværende dato på formatet DD-MM-YYYY
```

## Dykk dypere

Nå som vi har sett på noen enkle måter å få tak i nåværende dato i PHP, la oss gå litt dypere inn i hvordan dette fungerer. Som nevnt tidligere, returnerer funksjonen "date()" og "time()" dato og tid i et bestemt format. Dette formatet bestemmes av de forskjellige parametrene som kan gis til funksjonene. For eksempel, for å få datoen på formatet "MM-DD-YYYY", må du bruke "date('m-d-Y')".

I tillegg, hvis du ønsker å få tak i en spesifikk dato i fremtiden eller fortiden, kan du også legge til eller trekke fra antall dager, uker eller måneder til nåværende dato ved hjelp av "strtotime" funksjonen.

```PHP
$date = date("d-m-Y", strtotime('+1 week')); //Dette vil returnere datoen 1 uke frem i tid
```

Det er også viktig å merke seg at nåværende dato og tid er avhengig av serverinnstillingene. Dette betyr at hvis du trenger å bruke en spesifikk tidssone eller datoformat, må du sørge for å angi dette i koden din.

# Se også

- [Date og time funksjoner i PHP](https://www.php.net/manual/en/function.date.php)
- [DateTime klasse i PHP](https://www.php.net/manual/en/class.datetime.php)
- [strtotime funksjon i PHP](https://www.php.net/manual/en/function.strtotime.php)