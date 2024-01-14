---
title:                "Ruby: Hente gjeldende dato"
simple_title:         "Hente gjeldende dato"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor

Å få tak i den aktuelle datoen er en viktig ferdighet som mange programmerere trenger i sine programmer. Enten du skal vise datoen på en nettside, lage en kalender eller logge når en hendelse skjedde, er det avgjørende å kunne få tak i riktig dato. Heldigvis er dette en enkel oppgave i Ruby-programmering, og i denne bloggposten skal vi se nærmere på hvordan du kan gjøre det.

## Hvordan

For å få tak i den aktuelle datoen i Ruby, kan du bruke klassen `Date`. Denne klassen gir oss tilgang til ulike metoder for å få tak i datoen, og vi skal se på noen av dem her.

Først må vi importere `Date`-klassen:

```Ruby
require 'date'
```

Deretter kan vi få tak i datoen på ulike måter. Her er noen eksempler:

```Ruby
# Få tak i dagens dato
Date.today

# Få tak i datoen for en bestemt dag
Date.new(2021, 10, 25)

# Få tak i datoen for en bestemt dag fra en string
Date.parse("2021-10-25")
```

Du kan også få tak i ulike deler av datoen, som for eksempel år, måned og dag. Her er noen eksempler på hvordan du kan gjøre det:

```Ruby
# Få tak i året
Date.today.year

# Få tak i måneden
Date.today.month

# Få tak i dagen
Date.today.day
```

## Dypdykk

Nå som vi har sett hvordan du kan få tak i den aktuelle datoen, la oss se litt på hvordan det faktisk fungerer bak kulissene. Ruby bruker en metode kalt "proleptisk Gregorian calendar" for å håndtere datoene. Det betyr at selv om det ikke fantes en 0. år før vår tidsregning begynte, så bruker Ruby det allikevel for å gjøre beregningene enklere. I tillegg håndterer Ruby skuddår, så du trenger ikke bekymre deg for det når du jobber med datoer.

## Se også

- [Date-klassen i Ruby-dokumentasjonen](https://ruby-doc.org/stdlib-2.7.5/libdoc/date/rdoc/Date.html)
- [Ruby-forumet](https://www.ruby-forum.com/)
- [Ruby på nettet](https://www.ruby-lang.org/nb/)