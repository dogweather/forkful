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

---

## Hva & Hvorfor?

Beregning av en dato i fremtiden eller fortiden handler om å legge til eller trekke fra antall dager, måneder eller år fra en bestemt dato. Dette gjøres ofte av programmerere for å planlegge hendelser, opprette tidslinjer eller spore varigheten mellom to datoer.

## Hvordan:

Her er noen grunnleggende måter å beregne en dato i fremtiden eller fortiden på i Ruby:

```Ruby
require 'date'

dag_i_dag = Date.today
fremtidig_dato = dag_i_dag + 50
fortidig_dato = dag_i_dag - 30

puts "Dagens dato: #{dag_i_dag}"
puts "Fremtidig dato: #{fremtidig_dato}"
puts "Fortidig dato: #{fortidig_dato}"
```
Kjør programmet, og du vil se resultater som dette:

```Ruby
Dagens dato: 2022-10-04
Fremtidig dato: 2022-11-23
Fortidig dato: 2022-09-04
```

## Dypdykk:

Historie: I tidlige programmeringsspråk var datohåndtering ofte en kompleks oppgave som krevde mye arbeid. Med introduksjonen av innebygde dato- og tidsklasser i moderne programmeringsspråk som Ruby, har dette blitt betydelig enklere.

Alternativer: Selv om innebygd datofunksjonalitet er kraftig i Ruby, er det andre biblioteker som ActiveSupport (en del av Ruby on Rails) som tilbyr enda mer omfattende datohåndteringsfunksjoner.

Implementeringsdetaljer: Ruby bruker den Gregorianske kalenderen for alle datoberegninger. Når du legger til eller trekker fra dager, tar den hensyn til skuddår og antall dager i hver måned.

## Se også:

- Ruby Date Class Documentation (innbygd): https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html
- ActiveSupport Time Extensions (for mer avansert bruk): https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html
- "Programming Ruby: The Pragmatic Programmers' Guide" (for et dypere dykk): https://pragprog.com/titles/ruby/programming-ruby/