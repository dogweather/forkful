---
date: 2024-01-20 17:53:25.874914-07:00
description: "Utskrift for feils\xF8king lar deg vise data og programflyt i konsollen\
  \ for \xE5 forst\xE5 hva programmet ditt faktisk gj\xF8r. Programmerere bruker dette\
  \ til \xE5 spore\u2026"
lastmod: '2024-03-13T22:44:41.330864-06:00'
model: gpt-4-1106-preview
summary: "Utskrift for feils\xF8king lar deg vise data og programflyt i konsollen\
  \ for \xE5 forst\xE5 hva programmet ditt faktisk gj\xF8r."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

## What & Why?
Utskrift for feilsøking lar deg vise data og programflyt i konsollen for å forstå hva programmet ditt faktisk gjør. Programmerere bruker dette til å spore feil og sikre at koden oppfører seg som forventet.

## How to:
For å skrive ut debug-informasjon i Ruby, bruk `puts` eller `p`. Her er et enkelt eksempel:

```Ruby
def multipliser(a, b)
  result = a * b
  puts "Multiplisering av #{a} og #{b} gir: #{result}"
  result
end

multipliser(3, 7)
```

Output vil være:
```
Multiplisering av 3 og 7 gir: 21
```

Hvis du vil ha en mer detaljert utskrift av objekter, bruk `p`:

```Ruby
arr = ["eple", "banan", "kirsebær"]
p arr
```

Output vil da vise objektets rå form:
```
["eple", "banan", "kirsebær"]
```

## Deep Dive:
Før `puts` og `p` ble standard i Ruby, måtte feilsøking ofte gjøres med manuell inspeksjon eller ved hjelp av en debugger. Ruby's reflekterende egenskaper gir disse metodene, som gir detaljert og klar output.

Alternativt, kan du også bruke `print`, som er lik `puts` men legger ikke til en ny linje på slutten. Logger kan også være et alternativ for større prosjekter og kan inkludere detaljnivåer (INFO, WARNING, ERROR).

Bak kulissene konverterer `puts` argumentene sine til strenger med `to_s`, mens `p` bruker `inspect`, som ofte gir mer detaljer.

## See Also:
- [Ruby's Kernel#puts documentation](https://ruby-doc.org/core-2.7.0/IO.html#method-i-puts)
- [Ruby's Kernel#p documentation](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-p)
- [Ruby's Kernel#print documentation](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-print)
