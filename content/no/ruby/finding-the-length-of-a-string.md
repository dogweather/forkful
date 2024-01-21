---
title:                "Finn lengden på en streng"
date:                  2024-01-20T17:47:59.338866-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finne lengden til en streng er å telle antall tegn den inneholder. Programmerere gjør dette for validering, formatering eller andre logiske formål i kode.

## How to:
For å finne lengden av en streng i Ruby, bruk metoden `.length` eller `.size`.

```ruby
greeting = "Hei, verden!"
puts greeting.length  # Output: 13
puts greeting.size    # Output: 13
```

Disse to metodene gjør det samme – de returnerer antall tegn i strengen, inkludert mellomrom.

## Deep Dive
I de tidlige dagene av programmering, tok lagring av hvert tegn i en streng verdifullt minne og prosesseringsmakt. Å forstå strenglengde hjalp derfor med effektivitet. I moderne Ruby er `.length` og `.size` identiske metoder; bruk er basert på preferanse. Alternativt kan `.bytesize` metodene brukes for å få størrelsen av strengen i bytes, nyttig når du arbeider med ulike tegnsett og enkodinger.

Implementasjonen av disse metodene er direkte i Ruby’s kjernetolk, som gir en raskt tilgang til strengens lengde uten å måtte manuelt telle hvert tegn, noe som ville vært svært ineffektivt.

## See Also
- Ruby's documentation on strings: [ruby-doc.org/core-3.1.2/String.html](https://ruby-doc.org/core-3.1.2/String.html)
- A guide to Ruby strings for beginners: [learn.co/lessons/ruby-strings](https://learn.co/lessons/ruby-strings)