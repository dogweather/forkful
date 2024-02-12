---
title:                "Tekst zoeken en vervangen"
aliases: - /nl/ruby/searching-and-replacing-text.md
date:                  2024-01-28T22:07:22.197976-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Zoeken en vervangen van tekst is het verwisselen van specifieke woorden of zinnen met alternatieven in een string. Programmeurs doen dit om gegevens bij te werken, fouten te corrigeren of de opmaak te wijzigen - het houdt de zaken accuraat en opgeruimd.

## Hoe te:
Ruby maakt het makkelijk. Gebruik `gsub` om tekst wereldwijd te vervangen, of `sub` voor een enkele instantie. Hier is een snelle blik:

```ruby
# Oorspronkelijke string
phrase = "Hallo, wereld!"

# Vervang 'wereld' door 'Ruby'
puts phrase.gsub('wereld', 'Ruby')
# => Hallo, Ruby!

# Vervang alleen eerste voorkomen van 'l'
puts phrase.sub('l', '7')
# => Ha7lo, wereld!
```
De uitvoer? De eerste afdruk toont `"Hallo, Ruby!"`, de tweede geeft `"Ha7lo, wereld!"`.

## Diepgaande Duik
De methoden `gsub` en `sub` zijn al vroeg in de dagen van Ruby aanwezig, als spiegeling van het vervangingsconcept uit oudere talen zoals Perl. Alternatieven? Zeker, je zou een regex kunnen gebruiken voor meer complexe patronen, of zelfs `split` en `join` aan elkaar knopen als je je handig voelt.

Wat cool is, is Ruby's mogelijkheid om blokken te gebruiken met `gsub`. In plaats van een simpele zoek-en-vervang, kun je wat zwaarder werk verrichten binnen dat blok:

```ruby
# Elk woord met een hoofdletter
puts "maak mij mooi".gsub(/\b\w/) { |match| match.upcase }
# => Maak Mij Mooi
```

Waarom deze moeite? Om te beginnen laat het gebruik van regex met `gsub` je toe om genuanceerde gevallen aan te pakken waar je meer finesse nodig hebt dan simpelweg 'vind dit, vervang met dat'.

## Zie Ook
Scherp die vaardigheden aan - duik in de documentatie of bekijk deze bronnen:
- [Ruby String#gsub documentatie](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Reguliere Expressies in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)

Begrepen? Goed. Ga nu maar spelen met wat strings.
