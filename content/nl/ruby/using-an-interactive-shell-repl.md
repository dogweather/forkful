---
title:                "Het gebruik van een interactieve shell (REPL)"
date:                  2024-01-28T22:09:20.562018-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een interactieve shell, of REPL (Read-Eval-Print Loop), stelt je in staat om in real-time code te testen. Programmeurs gebruiken het om te experimenteren, fouten op te sporen, en de nuances van Ruby te leren zonder volledige scripts te hoeven maken.

## Hoe:
Ruby's REPL heet IRB (Interactive Ruby). Spring erin en probeer Ruby direct vanuit je terminal:

```Ruby
irb
2.7.0 :001 > puts "Hallo, Ruby wereld!"
Hallo, Ruby wereld!
 => nil
2.7.0 :002 > 5.keer { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby! => 5
```

## Diep Duiken
Geïntroduceerd in Ruby 1.8, is IRB een basisgereedschap voor Rubyisten. Het is geïnspireerd door de interactieve shells van Lisp en Python, en mengt experimenten met onmiddellijke feedback. Alternatieven zoals Pry bieden meer functies zoals syntaxiskleuring en een robuustere foutopsporingsomgeving. IRB zelf is eenvoudig maar kan worden uitgebreid met edelstenen zoals 'irbtools' om de functionaliteit uit te breiden. Hoe IRB de read-eval-print loop afhandelt, is door elke regel van de invoer te lezen, deze als Ruby-code te evalueren, en vervolgens het resultaat af te drukken, dit proces herhalend totdat men exit gebruikt.

## Zie Ook
- [Ruby's IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [De irbtools edelsteen](https://github.com/janlelis/irbtools)
