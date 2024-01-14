---
title:    "Fish Shell: Konvertere en dato til en streng"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en viktig funksjon i programmering, spesielt når man arbeider med dato- og tidsbaserte applikasjoner. Denne prosessen tillater deg å vise datoer på en klar og forståelig måte for brukere av programmet ditt. I denne bloggposten vil vi se på hvordan du kan gjøre dette ved hjelp av Fish Shell.

## Hvordan

For å konvertere en dato til en streng i Fish Shell, kan du bruke kommandoen `date` etterfulgt av `+%A, %d. %B %Y`. Dette vil gi deg datoen i et format som "Tirsdag, 28. september 2021".

```Fish Shell
date +%A, %d. %B %Y
```

Du kan også legge til klokkeslettet ved å bruke `%H:%M` i kommandoen. For eksempel, `date +%A, %d. %B %Y kl. %H:%M` vil gi deg "Tirsdag, 28. september 2021 kl. 10:30".

Her er et eksempel på hvordan dette ville se ut i et skript:

```Fish Shell
echo "Nåværende dato og tid er:"
echo (date +%A, %d. %B %Y kl. %H:%M)
```

### Mer informasjon

For å få en grundigere forståelse av hvordan `date` fungerer, kan du bruke kommandoen `man date` i terminalen. Dette vil gi deg dokumentasjonen for denne kommandoen og alle dens mulige formater.

En annen måte å spesifisere et format på er ved hjelp av variabelen `$__fish_date_format`. Du kan endre dette formatet ved å skrive `set -U __fish_date_format "%Y-%m-%d"` i terminalen. Dette vil endre datoformatet til å bli "2021-09-28".

## Se også

- [Fish Shell dokumentasjon om `date`](https://fishshell.com/docs/current/cmds/date.html)
- [Guide til å bruke dato og tid i Fish Shell](https://www.linux.com/topic/desktop/using-dates-date-command-linux/)
- [Mer informasjon om formatering av datoer i Fish Shell](https://stackoverflow.com/questions/18615585/how-to-format-current-date-time-using-a-shell-script-in-linux)