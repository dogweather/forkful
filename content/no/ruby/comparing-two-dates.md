---
title:                "Sammenligning av to datoer"
html_title:           "Ruby: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sammenligne to datoer er en vanlig oppgave for programmere. Dette innebærer å bestemme om en dato er før, lik eller etter en annen dato. Vi gjør dette for å kunne sortere datoer, bestemme forfallsdatoer, eller for å utføre andre operasjoner som involverer avhengighet av datoer.

## Slik gjør du det:
Å sammenligne to datoer i Ruby er ganske enkelt. Vi bruker metoden `#<=>` som returnerer enten -1, 0 eller 1 avhengig av om den første datoen er før, lik eller etter den andre datoen. For eksempel:

```Ruby
dob = Date.new(1990, 2, 8)
today = Date.today
dob <=> today # => -1
```
I dette tilfellet betyr -1 at `dob` er før `today`. Hvis de to datoene er like, vil metoden returnere 0, og hvis den første datoen er etter den andre, vil den returnere 1.

## Dykke dypere:
Metoden `#<=>` er en del av `Comparable` modulen i Ruby. Dette gjør det mulig å sammenligne ikke bare datoer, men også andre typer objekter, som strenger og tall. Alternativt kan vi også bruke `#==`, `#<`, `#<=`, `#>`, og `#>=` for å sammenligne datoer og få et boolsk svar, true eller false. Datoer kan også sammenlignes ved å konvertere dem til `Time` objekter og bruke `#<=>` eller `#is_before?` og `#is_after?` metoder.

## Se også:
- [Official Ruby documentation for Date class](https://ruby-doc.org/stdlib-2.7.4/libdoc/date/rdoc/Date.html#method-i-3C-3D-3E)
- [The Comparable Module in Ruby](https://www.techotopia.com/index.php/The_Comparable_Module_in_Ruby)
- [Ruby Date and Time Comparison](https://www.go4expert.com/articles/ruby-date-time-comparison-t879/)