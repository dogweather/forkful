---
title:                "Konvertering av en streng til små bokstaver"
html_title:           "Ruby: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr at alle bokstavene i en tekst blir endret til små bokstaver. Dette er nyttig for å sikre at teksten er konsekvent og enkelt å behandle for programmerere.

## Slik gjør du det:
```ruby
tekst = "HELLO WORLD"
puts tekst.downcase
```
Output: hello world

## Dypdykk:
Konvertering av strenger til små bokstaver har vært en vanlig programmeringsøvelse siden starten av datamaskinens tid. Tidligere måtte programmører manuelt endre bokstavene for å sikre at teksten var konsekvent. Nå er det heldigvis en enkel funksjon som gjør dette for oss. Det finnes også alternative metoder for å konvertere strenger til små bokstaver, for eksempel ved hjelp av regular expressions. Konverteringsprosessen varierer også avhengig av programmeringsspråk.

## Se også:
- Ruby dokumentasjon for String#downcase: https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase
- "How to convert a string to lowercase in Ruby" (Stack Overflow): https://stackoverflow.com/questions/10262235/how-to-convert-a-string-to-lowercase-in-ruby