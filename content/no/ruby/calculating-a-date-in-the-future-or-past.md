---
title:                "Ruby: Beregning av en dato i fremtiden eller fortiden"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor bør man kunne regne ut datoer i fremtiden eller fortiden? Det kan være nyttig for å planlegge fremtidige arrangementer eller for å spore tilbake i tid.

# Hvordan gjøre det

For å regne ut en dato i fremtiden eller fortiden, kan man bruke Ruby sin `Date` klasse. Ved å bruke `#new` metoden og gi den ønsket år, måned og dag, kan man opprette en ny dato.

```ruby
# Opprett en dato for 1. mai 2022
future_date = Date.new(2022, 5, 1)

# Opprett en dato for 1. mai 2018
past_date = Date.new(2018, 5, 1)
```

For å regne ut en dato i fremtiden eller fortiden, kan man bruke `#next_day` eller `#prev_day` metoden. Disse metodene tar et heltall som argument, som representerer hvor mange dager man vil gå frem eller tilbake fra den opprinnelige datoen.

```ruby
# Regn ut datoen 30 dager etter 1. mai 2022
future_date = future_date.next_day(30)

# Regn ut datoen 30 dager før 1. mai 2018
past_date = past_date.prev_day(30)
```

Man kan også bruke `#next_month` og `#prev_month` metoden for å regne ut datoer i fremtiden eller fortiden basert på antall måneder i stedet for dager.

```ruby
# Regn ut datoen 6 måneder etter 1. mai 2022
future_date = future_date.next_month(6)

# Regn ut datoen 6 måneder før 1. mai 2018
past_date = past_date.prev_month(6)
```

For å få utskrift av datoen i ønsket format, kan man bruke `#strftime` metoden og gi den ønsket formatstring som argument.

```ruby
# Få utskrift av datoen i formatet dd.mm.åååå
puts future_date.strftime("%d.%m.%Y")

# Utskrift: 31.10.2022

# Få utskrift av datoen i formatet åååå/mm/dd
puts past_date.strftime("%Y/%m/%d")

# Utskrift: 2018/11/01
```

# Dypdykk

Det er også mulig å regne ut datoer i fremtiden eller fortiden fra en eksisterende dato. Ved å bruke `#next` eller `#prev` metoden, kan man regne ut datoer basert på en annen dato. I tillegg kan man også bruke `#next_year` og `#prev_year` metoden for å regne ut datoer som er ett år frem eller tilbake fra en eksisterende dato.

```ruby
# Opprett en eksisterende dato
existing_date = Date.new(2020, 6, 15)

# Regn ut datoen 2 år etter den eksisterende datoen
future_date = existing_date.next_year(2)

# Regn ut datoen 10 år før den eksisterende datoen
past_date = existing_date.prev_year(10)
```

Man kan også bruke `#next_week` eller `#prev_week` metoden for å regne ut datoer basert på antall uker i stedet for dager. Og ved å bruke `DateTime` klasse i stedet for `Date` klasse, kan man også inkludere klokkeslett i utregningen av datoer.

# Se også

- [Ruby Doc - Date Class](https://ruby-doc.org/core-2.7.1/Date.html)
- [Ruby Doc - DateTime Class](https://ruby-doc.org/standard-library-2.7.1/libdoc/date/rdoc/DateTime.html)
- [Ruby Doc - String#strftime](https://ruby-doc.org/core-2.7.1/String.html#method-i-strftime)