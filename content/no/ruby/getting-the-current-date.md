---
title:                "Å få dagens dato"
html_title:           "Ruby: Å få dagens dato"
simple_title:         "Å få dagens dato"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange tilfeller hvor du kanskje trenger å ha den nåværende datoen i et Ruby-program. Kanskje du vil vise datoen til brukeren, lagre en transaksjonsdato eller lage en dynamisk filnavn med den nåværende datoen.

## Hvordan få nåværende dato
Det er enkelt å få den nåværende datoen i Ruby ved hjelp av `Time`-klassen. Her er et eksempel på å få nåværende dato og lagre den i en variabel:

```Ruby
nåværende_dato = Time.now
```

Du kan også tilpasse utskriften av datoen ved å bruke `strftime`-metoden og angi ønsket format. For eksempel:

```Ruby
nåværende_dato.strftime("%d.%m.%Y") # => 04.05.2021
```

## Dypdykk
Nåværende dato er egentlig en datastruktur kalt `Time` som inneholder informasjon om dato, tid og tidssone. Du kan få tilgang til forskjellig informasjon om datoen, for eksempel dag, måned, år, time, minutt og sekunder.

```Ruby
nåværende_dato.day # => 4
nåværende_dato.month # => 5
```

I tillegg kan du også utføre matematiske operasjoner på datoer, for eksempel å legge til eller trekke fra et antall dager.

```Ruby
neste_uke = nåværende_dato + 7 # legger til 7 dager
forrige_måned = nåværende_dato - 30 # trekker fra 30 dager
```

## Se også
- Ruby Time-klassen dokumentasjon: https://ruby-doc.org/core-3.0.1/Time.html
- Enkel guide til bruk av Time-klassen i Ruby: https://www.rubyguides.com/2019/02/ruby-time-class/