---
date: 2024-01-20 17:42:52.951576-07:00
description: "How to: Sletting av tegn i Ruby har solid st\xF8tte gjennom metoder\
  \ som `delete` og `gsub`. Metoden `delete` er rett fram - den fjerner konkret angitte\
  \ tegn.\u2026"
lastmod: '2024-04-05T22:50:55.315527-06:00'
model: gpt-4-1106-preview
summary: "Sletting av tegn i Ruby har solid st\xF8tte gjennom metoder som `delete`\
  \ og `gsub`."
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## How to:
Slette med `delete` og `gsub`:

```ruby
# Enkel sletting med `delete`:
streng = "Heisann! Hvordan går det?"
renset_streng = streng.delete "!?"
puts renset_streng
# Output: Heisann Hvordan går det

# Mønsterbasert sletting med `gsub`:
epost = "eksempel@eksempel.com"
renset_epost = epost.gsub(/[aeiou]/, "")
puts renset_epost
# Output: ksmpl@ksmpl.cm
```

## Deep Dive
Sletting av tegn i Ruby har solid støtte gjennom metoder som `delete` og `gsub`. Metoden `delete` er rett fram - den fjerner konkret angitte tegn. `gsub` går dypere; den støtter regulære uttrykk, som gjør komplekse mønstre mulige å fjerne.

Metoder som `tr` og `squeeze` er også nyttige - `tr` erstatter tegn, mens `squeeze` fjerner duplikater. I historisk kontekst har Ruby alltid vært sterk på tekstbehandling, noe som gjør det til et yndet språk for skripting og datahåndtering.

Når det gjelder implementasjon, arbeider `gsub` ved å scanne strengen og utføre en erstatning for hvert mønster som matcher, mens `delete` kun ser etter eksakte tegnmatcher og fjerner disse.

## See Also
For mer om tekstmanipulering:

- Ruby-doc for `String#delete`: https://ruby-doc.org/core/String.html#method-i-delete
- Ruby-doc for `String#gsub`: https://ruby-doc.org/core/String.html#method-i-gsub
- Regulære uttrykk i Ruby: https://ruby-doc.org/core/Regexp.html
