---
title:    "Fish Shell: Utskrift av feilsøkingsdata"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Hvorfor

Hvis du er en utvikler som arbeider med Fish Shell, kjenner du sannsynligvis allerede til nytten av å bruke debug output i programmene dine. Men for de nye til dette språket, kan det hende at du lurer på hvorfor du i det hele tatt skulle bry deg om dette. Vel, debugging kan være en av de mest effektive måtene å finne og løse feil i koden din på. Det er en måte å spore gjennomgangen til programmet og sjekke verdier på forskjellige punkter i koden for å forstå hva som skjer og identifisere hvor potensielle problemer kan oppstå. Nå som du vet hvorfor dette er viktig, la oss se på hvordan du kan implementere debugging-teknikker i Fish Shell.

# Slik gjør du det

For å aktivere debugging i Fish Shell, må du bruke "set"-kommandoen med "fish_debug" flagget. Dette vil aktivere debugmodus og skrive ut output til terminalen. Her er et eksempel på Fish Shell-kode som demonstrerer dette:

```
set -x fish_debug
echo "Hei, verden!"
```

Når du kjører dette i terminalen, vil du se en ekstra linje med output som viser hva som blir evaluert fra koden din:

```
+ echo "Hei, verden!"
Hei, verden!
```

Dette gjør det enklere å forstå hva som skjer i koden din og hvor eventuelle problemer kan oppstå. Sørg for å fjerne "fish_debug" flagget når du er ferdig med debugging, ellers vil du få ekstra output hver gang du kjører koden din.

# Dykk dypere

Hvis du ønsker å ta debugging til et høyere nivå, kan du også bruke kommandoen "set -x", som vil aktivere tracing-modus. Dette vil skrive ut hver eneste linje med koden din, slik at du kan følge med på hva som skjer i detalj. Her er et eksempel på bruk av denne kommandoen:

```
set -x
echo "Dette er min debugmelding."
```

Output vil da se slik ut:

```
+ echo "Dette er min debugmelding."
+ echo "Dette er min debugmelding."
Dette er min debugmelding.
```

Som du kan se, skrives både koden og output ut i terminalen. Dette kan være nyttig hvis du trenger å finne feil på et svært detaljert nivå.

# Se også

- [Fish Shell nettside] (https://fishshell.com)
- [Fish Shell dokumentasjon] (https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub-repositorie] (https://github.com/fish-shell/fish-shell)