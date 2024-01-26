---
title:                "Søking og erstatting av tekst"
date:                  2024-01-20T17:58:45.246700-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
("Hva & Hvorfor?")
Tekstsøk og -erstattning lar oss finne og bytte ut spesifikke ord eller fraser i en streng. Programmerere bruker dette til å endre kode, manipulere data, og automatisere redigering.

## How to:
("Slik gjør du:")
Ruby gjør det enkelt med `gsub` metoden. `gsub` står for "global substitution" og bytter alle treff.

```Ruby
original_text = "Gledelig jul og et godt nyttår!"
replacement_text = original_text.gsub('jul', 'påske')
puts replacement_text
```

Output:
```
Gledelig påske og et godt nyttår!
```

Ønsker du kun å erstatte det første treffet, bruk `sub` istedenfor `gsub`.

```Ruby
original_text = "Jul, søte jul ikke vekk dette forvirrende jul!"
replacement_text = original_text.sub('jul', 'påske')
puts replacement_text
```

Output:
```
Påske, søte jul ikke vekk dette forvirrende jul!
```

## Deep Dive
("Dypdykk")
I gamle dager måtte tekstendringer gjøres manuelt eller med komplekse skript. `gsub` og `sub` i Ruby er inspirert av tidligere tekstbehandlingsverktøy som 'sed' i Unix.

Alternativer til `gsub` inkluderer regex literals for komplekse søkemønstre:

```Ruby
replacement_text = original_text.gsub(/jul/i, 'påske')
```

Her ignorerer `/i` store/små bokstaver. Implementasjonsdetaljer som disse tilbyr fleksibilitet og kraft.

## See Also
("Se også")
- Ruby-doc for `String#gsub` og `String#sub`: [https://ruby-doc.org/core](https://ruby-doc.org/core)
- Regex-basert søk: [https://ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)
- 'sed' kommandoorientert tekstbehandler: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
