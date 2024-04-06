---
date: 2024-01-20 17:58:45.246700-07:00
description: "How to: (\"Slik gj\xF8r du:\") Ruby gj\xF8r det enkelt med `gsub` metoden.\
  \ `gsub` st\xE5r for \"global substitution\" og bytter alle treff."
lastmod: '2024-04-05T21:53:42.262490-06:00'
model: gpt-4-1106-preview
summary: "(\"Slik gj\xF8r du:\") Ruby gj\xF8r det enkelt med `gsub` metoden."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

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
