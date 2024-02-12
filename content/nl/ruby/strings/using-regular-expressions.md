---
title:                "Reguliere expressies gebruiken"
aliases:
- /nl/ruby/using-regular-expressions/
date:                  2024-01-28T22:09:40.202043-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies (regex) zijn patronen die worden gebruikt om combinaties van karakters in strings te vinden. Programmeurs gebruiken ze voor het zoeken, bewerken of valideren van tekst omdat ze nauwkeurig en efficiënt zijn.

## Hoe te:
Laten we enkele basisprincipes van Ruby regex doornemen.

```Ruby
# Een overeenkomst vinden
zin = "Hallo, Wereld!"
puts zin.match(/Wereld/) # Uitvoer: Wereld

# Vervanging
puts zin.gsub(/Wereld/, "Ruby") # Uitvoer: Hallo, Ruby!

# Overeenkomsten extraheren
email = "contact@voorbeeld.com"
puts email.match(/\A[^@\s]+@([^@\s]+\.)+[^@\s]+\z/).to_s # Uitvoer: contact@voorbeeld.com

# Itereren over overeenkomsten
"Frodo, Gandalf, Arwen".scan(/\w+/) { |naam| puts naam }
# Uitvoer:
# Frodo
# Gandalf
# Arwen
```

## Diepere duik
Reguliere expressies in Ruby zijn beïnvloed door Perl's sterke regex-mogelijkheden. Alternatieven voor regex zijn stringmethoden zoals `#include?`, `#start_with?` en `#end_with?`, maar geen biedt dezelfde kracht en flexibiliteit. Ruby implementeert regex met behulp van zijn eigen bibliotheek die is afgeleid van Perl's regex-engine, wat functies biedt zoals vooruit en achteruit kijken, niet-gretige overeenkomsten en snelkoppelingen voor tekenklassen.

## Zie ook
- [Ruby Reguliere Expressies](https://ruby-doc.org/core-3.1.0/Regexp.html): Officiële Ruby-documentatie voor regex.
- [Rubular](http://rubular.com/): Een op Ruby gebaseerde reguliere expressie-editor, goed voor het testen van patronen.
