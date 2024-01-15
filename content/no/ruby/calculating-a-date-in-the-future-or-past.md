---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "Ruby: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du planlegger fremover i tid eller ønsker å vite hva datoen var for en hendelse i fortiden, er det nyttig å kunne beregne en dato i fremtiden eller fortiden. Med Ruby kan du enkelt finne ut hvilken dato som vil være for eksempel 30 dager fra nå, eller hvilken dato som var to uker siden.

## Hvordan

For å beregne en dato i fremtiden eller fortiden i Ruby, bruker vi metoden `strftime` og `Time`-klassen. Her er et eksempel på hvordan vi kan finne ut hvilken dato som er 30 dager fra nå:

```ruby
future_date = Time.now + (30 * 24 * 60 * 60)
puts future_date.strftime("%d %b %Y")
# Output: 03 Mar 2021
```

La oss nå se på et eksempel der vi ønsker å finne ut hva datoen var to uker siden:

```ruby
past_date = Time.now - (14 * 24 * 60 * 60)
puts past_date.strftime("%d %b %Y")
# Output: 08 Feb 2021
```

Som du ser i eksemplene ovenfor, kan vi bruke `strftime`-metoden til å formatere datoen slik vi ønsker det. Mer om ulike formateringsalternativer kan du lese om i [Ruby sin dokumentasjon for `strftime`](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime).

## Dykk dypere

Det er viktig å vite at når vi beregner en dato i Ruby, får vi datoen i forhold til nåværende tidssone. Dette kan føre til at datoen kan variere avhengig av hvor brukeren befinner seg. For å unngå dette, kan vi bruke `utc`-metoden for å få datoen i UTC-tidssone.

En annen ting å være oppmerksom på er at når vi bruker `Time`-klassen, får vi også informasjon om klokkeslettet i tillegg til datoen. Hvis du bare er interessert i datoen, kan du bruke `Date`-klassen i stedet.

## Se også

- [Ruby sin dokumentasjon for `strftime`](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Mer om `Time`-klassen i Ruby](https://ruby-doc.org/core-3.0.0/Time.html)
- [Mer om `Date`-klassen i Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)