---
date: 2024-01-26 04:17:19.027945-07:00
description: "En interaktiv skall, eller REPL (Read-Eval-Print Loop), lar deg teste\
  \ kode i sanntid. Programmerere bruker den til \xE5 eksperimentere, feils\xF8ke,\
  \ og l\xE6re\u2026"
lastmod: '2024-03-13T22:44:41.329917-06:00'
model: gpt-4-0125-preview
summary: "En interaktiv skall, eller REPL (Read-Eval-Print Loop), lar deg teste kode\
  \ i sanntid. Programmerere bruker den til \xE5 eksperimentere, feils\xF8ke, og l\xE6\
  re\u2026"
title: Bruke et interaktivt skall (REPL)
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En interaktiv skall, eller REPL (Read-Eval-Print Loop), lar deg teste kode i sanntid. Programmerere bruker den til å eksperimentere, feilsøke, og lære Ruby's nyanser uten å lage fullstendige skript.

## Hvordan:
Ruby's REPL kalles IRB (Interactive Ruby). Hopp inn og prøv Ruby direkte fra terminalen din:

```Ruby
irb
2.7.0 :001 > puts "Hallo, Ruby-verden!"
Hallo, Ruby-verden!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Dypdykk
Introdusert i Ruby 1.8, IRB er en stift for Rubyister. Den er inspirert av de interaktive skalene til Lisp og Python, som smelter sammen eksperimentering med umiddelbar tilbakemelding. Alternativer som Pry tilbyr flere funksjoner som syntaksutheving og et mer robust feilsøkingsmiljø. IRB i seg selv er enkel, men kan utvides med gems som 'irbtools' for å utvide funksjonaliteten. Hvordan IRB håndterer read-eval-print-løkken er ved å lese hver linje med inndata, evaluere den som Ruby-kode, og deretter skrive ut resultatet, og loope denne prosessen til man avslutter.

## Se Også
- [Ruby's IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [The irbtools gem](https://github.com/janlelis/irbtools)
