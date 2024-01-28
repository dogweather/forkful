---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:17:24.407303-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Ett interaktivt skal, eller REPL (Read-Eval-Print Loop), låter dig testa kod i realtid. Programmerare använder det för att experimentera, felsöka och lära sig Ruby's nyanser utan att skapa fullständiga skript.

## Hur man:
Ruby's REPL kallas IRB (Interactive Ruby). Hoppa in och testa Ruby direkt från din terminal:

```Ruby
irb
2.7.0 :001 > puts "Hej, Ruby-världen!"
Hej, Ruby-världen!
 => nil
2.7.0 :002 > 5.times { print "Ruby! " }
Ruby! Ruby! Ruby! Ruby! Ruby!  => 5
```

## Djupdykning
Introducerad i Ruby 1.8, är IRB ett måste för Rubyister. Det är inspirerat av de interaktiva skalen från Lisp och Python, och kombinerar experiment med omedelbar återkoppling. Alternativ som Pry erbjuder fler funktioner som syntaxmarkering och en robustare felsökningsmiljö. IRB i sig är enkelt, men kan utökas med gems som 'irbtools' för att utöka funktionaliteten. Hur IRB hanterar läs-eval-print-loopen är genom att läsa varje rad av input, utvärdera den som Ruby-kod och sedan skriva ut resultatet, och loopa denna process tills utgång.

## Se också
- [Ruby's IRB](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [Gemet irbtools](https://github.com/janlelis/irbtools)
