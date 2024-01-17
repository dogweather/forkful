---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Ruby: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Ruby: Enkelt metode for å beregne datoer i fremtiden eller fortiden

## Hva & Hvorfor?
Beregning av datoer i fremtiden eller fortiden er en vanlig oppgave for programvareutviklere. Dette innebærer å finne ut en dato basert på en gitte antall dager, uker, måneder eller år som skal legges til eller trekkes fra en annen dato.

Dette er nyttig for å lage funksjoner som viser for eksempel fødselsdager, kommende arrangementer og andre hendelser. Det er også praktisk for å lage tidsplaner og timerplaner i applikasjoner som involverer datoer.

## Slik gjør du det:
For å beregne en dato i fremtiden eller fortiden, kan du bruke Ruby's fleksible og intuitive `Date`-klasser. Her er noen eksempler på hvordan du kan bruke denne funksjonen:

Ruby-kodeblokk med eksempler:

```Ruby
# Beregning av datoer i fremtiden
puts Date.today + 1 # Legger til en dag til dagens dato
# Output: 2021-06-02

puts Date.today + 7 # Legger til en uke til dagens dato
# Output: 2021-06-08

puts Date.today + 30 # Legger til en måned til dagens dato
# Output: 2021-07-01

# Beregning av datoer i fortiden
puts Date.today - 1 # Trekker fra en dag fra dagens dato
# Output: 2021-05-31

puts Date.today - 7 # Trekker fra en uke fra dagens dato
# Output: 2021-05-25

puts Date.today - 30 # Trekker fra en måned fra dagens dato
# Output: 2021-05-02
```

Som du kan se, er beregning av datoer i fremtiden eller fortiden enkelt og intuitivt med Ruby.

## Dypdykk:
Denne funksjonen i Ruby ble introdusert i versjon 1.8.7 og er en del av standardbiblioteket. Det er mange alternativer for å beregne datoer i fremtiden eller fortiden, men Ruby's `Date`-klasser skiller seg ut for sin enkelhet og funksjonalitet.

Implementeringen av denne funksjonen i Ruby bruker den gregorianske kalenderen, som er den vanligste kalenderen som brukes i dag. Hvis du trenger å bruke en annen kalender, kan du bruke Ruby's `Calendar`-modul.

## Se også:
- [Ruby's offisielle dokumentasjon om `Date`-klassen] (https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Ruby on Rails Tutorial: Working with Dates] (https://www.railstutorial.org/book/modeling_users#sec-date_formats)
- [Ruby Guides: Working with Dates and Times] (https://www.rubyguides.com/working-with-dates/)
- [Date Calculations in Ruby] (https://medium.com/analytics-vidhya/date-calculations-in-ruby-56faed1760e6)