---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Søk og Erstatt Tekst med Ruby

## Hva & Hvorfor?
"Søk og erstatt" er en prosess der spesifikke strenger i en blokk med tekst blir identifisert (søk) og deretter endret til noe annet (erstatt). Dette er nyttig for programmerere for å manipulere data, fikse feil, og tilpasse output.

## Hvordan:
Her viser vi typiske bruksområder med [Rubys innebygde metoder](https://ruby-doc.org/core-2.7.0/String.html):

Så, for å søke og erstatte tekst i Ruby, kan vi bruke `gsub` metode. Se på dette eksempelet:

```Ruby
tekst = "Hei, Hvordan går det?"
print tekst.gsub('Hvordan', 'Hva')
```

Output vil være: `"Hei, Hva går det?"`

Du kan også bruke et regulært uttrykk (regex) med `gsub` for mer avanserte søk. La oss søke etter hvert ord på 5 bokstaver og erstatte det med '####':

```Ruby
tekst = "Hello world from the other side"
print tekst.gsub(/\b\w{5}\b/, '####')
```

Output vil være: `"Hello world from the #### side"`

## Dypdykk
Historisk sett har søk og erstatt alltid vært en grunnleggende funksjon i tekstbehandling og programmering. Dens utførelse og implementering har variert fra språk til språk, men essensen har forblitt den samme.

I Ruby, `gsub` (hvilken står for Global Substitution) er en av de mest brukte metodene for denne jobben. Ut over `gsub`, kan vi også bruke `gsub!`, som endrer strengen direkte i stedet for å returnere en ny.

Det finnes flere alternativer for å søke og erstatte tekst i Ruby, inkludert `tr`, `tr_s`, og `delete`. Valget mellom disse avhenger av dine behov og preferanser.

## Se Også
- Ruby Docs String Methods: [https://ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- Tutorial: [https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)
- In-Depth LevelUp Article on gsub: [https://www.rubyguides.com/2019/07/ruby-gsub-method/](https://www.rubyguides.com/2019/07/ruby-gsub-method/)