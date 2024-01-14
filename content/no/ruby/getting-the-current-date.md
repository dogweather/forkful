---
title:    "Ruby: Å få den nåværende datoen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen i et Ruby-program kan være en viktig og nyttig funksjon for å øke nøyaktigheten og funksjonaliteten til programmet ditt. Enten du trenger det for å logge hendelser, sette opp automatisk genererte rapporter, eller bare for å vise den aktuelle datoen i brukergrensesnittet, er å kunne få den nåværende datoen en viktig ferdighet å ha når du koder i Ruby.

## Hvordan få den nåværende datoen i Ruby

For å få den nåværende datoen i Ruby, bruker du en kombinasjon av Ruby sin innebygde `Time` klasse og `strftime` metoden. Først lager du et nytt `Time` objekt ved å kalle `Time.now` metoden. Deretter bruker du `strftime` metoden for å formatere datoen slik du ønsker det. For eksempel:

```Ruby
now = Time.now
puts now.strftime("%d/%m/%Y")
```

Dette vil resultere i output som ser omtrent slik ut: `12/03/2021`. Du kan også legge til forskjellige formateringsparametere for å få ønsket output, for eksempel `%B` for å få måneden skrevet ut som navn, eller `%H:%M` for å få tiden i timer og minutter.

## Dykk dypere ned i å få den nåværende datoen

Hvis du ønsker å få en mer detaljert forståelse av hvordan `Time` og `strftime` fungerer i Ruby, kan du utforske følgende konsepter og funksjoner:

1. Timezones: Ved å bruke `Time.now` vil du få datoen og tiden i din lokale tidssone. Hvis du ønsker å få datoen i en annen tidssone, kan du bruke `Time.now.getlocal` og legge til tidssonen som et argument.
2. Andre formateringsparametere: Det finnes en rekke andre formateringsparametere du kan bruke med `strftime` for å få ønsket output, for eksempel `%A` for å få ukedagen skrevet ut som navn.
3. Ytelse: Å bruke `Time.now` for å få datoen kan være en ressurskrevende operasjon, spesielt hvis du bruker den flere ganger i løpet av programmet. I slike tilfeller kan det være bedre å bruke `DateTime.now` som er en mer lettvektig versjon av `Time.now`.

## Se også

* [Ruby sin offisielle dokumentasjon om Time og Datetime](https://ruby-doc.org/core-3.0.0/Time.html)
* [Enkel guide til formatering av dato og tid i Ruby](https://dev.to/shayanypn/using-datetime-in-ruby-zkf)
* [Hvordan håndtere tidssoner i Ruby](https://thoughtbot.com/blog/how-to-handle-time-zones-in-ruby)