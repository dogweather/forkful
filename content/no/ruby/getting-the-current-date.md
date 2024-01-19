---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente den nåværende dato handler om å hente live tidspunktdata fra systemet ditt. Dette er kritisk for programmerere fordi det hjelper å spore begivenheter, logge oppføring, og tilpasse brukeropplevelser ved å forstå tid og dato.

## Hvordan:
Her er hvordan du kan hente den nåværende datoen i Ruby:

```Ruby
# Regulær dato i Ruby
nå = Time.now
puts nå
```

Dette utgir: 

```Ruby
2022-04-22 12:34:56 +0200
```

Datoen kan formateres slik du vil ha det:

```Ruby
dato = Time.now.strftime("%d/%m/%Y")
puts dato
```

Dette utgir: 

```Ruby
22/04/2022
```

## Dyp Dykk
Historisk sett har Ruby brukt Time klasse for å hente den nåværende tiden. Men senere versjoner brukt Date og DateTime klassene, inkludert 'date' biblioteket for mer funksjonalitet.

Alternativt kan du bruke 'time' gem for mer funksjonell og presis tidshåndtering.

Når det gjelder implementeringsdetaljer, bruker Ruby faktisk systemets tid, tilgjengelig via kernel, for å generere det nåværende tidspunktet. Dette er fortolket av Time klasse som gir flere metoder for formatering og håndtering.

## Se Også
For mer informasjon om dato- og tidsprosessering i Ruby, se disse ressursene:
1. Ruby offisiell dokumentasjon: [Time Class](https://ruby-doc.org/core-2.7.0/Time.html)
2. RubyGems: [Time gem](https://rubygems.org/gems/time)
3. Tutuorial: [Ruby Date & Time Handling](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)