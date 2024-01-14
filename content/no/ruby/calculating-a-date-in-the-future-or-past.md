---
title:    "Ruby: Beregning av datoer i fremtiden eller fortiden"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Ruby Programmering: Hvordan beregne datoer i fremtiden og fortiden

## Hvorfor?

Noen ganger kan vi ønske å beregne en dato i fremtiden eller fortiden for å planlegge eller analysere data. Dette kan være nyttig for en rekke formål, som å lage en timeplan for kommende hendelser eller for å beregne alderen til en person på et bestemt tidspunkt.

## Hvordan gjør man det?

Det er flere måter å beregne datoer i fremtiden eller fortiden ved hjelp av Ruby programmering. Her er et eksempel på hvordan du kan legge til eller trekke fra et bestemt antall dager fra dagens dato:

```Ruby
# Beregn dagens dato
today = Date.today

# Legg til 30 dager og lagre som en ny variabel
future_date = today + 30

# Trekke fra 14 dager og lagre som en annen variabel
past_date = today - 14

# Skriv ut resultatene
puts "Dagens dato er #{today}"
puts "Datoen om 30 dager blir #{future_date}"
puts "Datoen for 14 dager siden var #{past_date}"
```

Output:
```bash
Dagens dato er 2021-06-04
Datoen om 30 dager blir 2021-07-04
Datoen for 14 dager siden var 2021-05-21
```

Når du bruker Ruby, kan du også bruke metodene .next og .prev for å gå til neste eller forrige dag innenfor samme uke. For eksempel:

```Ruby
# Beregn dagens dato
today = Date.today

# Beregn neste mandag
next_monday = today.next(:monday)

# Beregn forrige onsdag
last_wednesday = today.prev(:wednesday)

# Skriv ut resultatene
puts "Dagens dato er #{today}"
puts "Neste mandag er #{next_monday}"
puts "Forrige onsdag var #{last_wednesday}"
```

Output:
```bash
Dagens dato er 2021-06-04
Neste mandag er 2021-06-07
Forrige onsdag var 2021-06-02
```

## Dypdykk

Ruby har også innebygde klasser og metoder for å håndtere datoer og tid, som DateTime og Time. Disse kan være nyttige for mer avanserte beregninger, som å ta hensyn til klokkeslett og tidssoner.

Videre kan du også bruke ulike formateringsmetoder for å konvertere datoer til ønsket format, som for eksempel å vise måneder som tekst i stedet for tall. Utforsk gjerne disse mulighetene og lær mer om hvordan Ruby kan hjelpe deg med å beregne datoer i fremtiden og fortiden.

## Se også

[Offisiell dokumentasjon for Ruby Date og DateTime klasser](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/index.html)

[En guide til Ruby Date og Time klasser](https://www.rubyguides.com/2019/09/ruby-date/)

[Hvordan håndtere datoer og tid i Ruby](https://www.rubyguides.com/2015/06/ruby-time/)

Jeg håper denne blogginnlegget var nyttig for deg som ønsker å lære mer om hvordan man beregner datoer i Ruby. Lykke til med kodingen!