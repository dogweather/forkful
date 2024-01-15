---
title:                "Uthenting av delstrenger"
html_title:           "Ruby: Uthenting av delstrenger"
simple_title:         "Uthenting av delstrenger"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger når vi jobber med tekst i Ruby, trenger vi å trekke ut deler av en streng for å bruke den i vårt program. Dette kan være nyttig når vi ønsker å manipulere data eller få spesifikke deler av teksten.

## Hvordan

Vi kan trekke ut substrings fra en streng ved hjelp av Ruby's `slice` eller `[]` metode. La oss se på et enkelt eksempel:

```Ruby
text = "Dette er en tekststreng"
substring = text.slice(6, 12)

puts substring
# Output: er en tekst
```

Vi bruker `slice` for å trekke ut tekst fra posisjon 6 til 12, og lagrer den i variabelen `substring`. Vi kan også bruke `[]` notasjon for å oppnå det samme resultatet:

```Ruby
text = "Dette er en tekststreng"
substring = text[6..17]

puts substring
# Output: er en tekst
```

Denne gangen bruker vi `[]` notasjon og spesifiserer start- og sluttposisjonen for substringen. Du kan også bruke et enkelt tall som posisjon, og da vil det hente ut enkelt karakter fra strengen.

## Dypdykk

Vi kan også bruke `slice` og `[]` metoden med et negativt tall som et argument. Dette vil trekke ut tekst fra slutten av strengen. La oss se på et eksempel:

```Ruby
text = "Dette er en tekststreng"
substring = text.slice(-9..-1)

puts substring
# Output: tekststreng
```

Her bruker vi et negativt tall for å velge de siste 9 tegnene fra strengen.

Vi kan også bruke `slice` og `[]` sammen med andre metoder som `gsub` (erstatte deler av en streng) og `split` (splitte strengen inn i en array). Her er et eksempel som kombinerer disse metodene:

```Ruby
text = "Dette er en tekststreng"
substring = text.gsub("tekststreng", "viktig tekst")
puts substring.split(" ")[-2]
# Output: viktig
```

Vi bruker `gsub` for å erstatte "tekststreng" med "viktig tekst", og deretter bruker vi `split` for å dele den resulterende strengen inn i en array. Her bruker vi så `-2` som posisjon for å få det nest siste elementet i arrayen, som er "viktig".

## Se også

- [Ruby dokumentasjon for `slice` metoden](https://ruby-doc.org/core-3.0.1/String.html#method-i-slice)
- [Ruby dokumentasjon for `[]` notasjon](https://ruby-doc.org/core-3.0.1/String.html#method-i-5B-26-5D)