---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# Ruby: Skriv ut Debug Output
Lär dig hur du kan använda Ruby för att skriva ut debug information direkt i konsolen.

## Vad & Varför?
Att skriva ut debug output handlar om att få datorn att visa information om vilka processer som körs och vad de gör. Detta är ett hårdvaruprogrammerarens nödvändiga verktyg för att hitta och fixafel i sin kod.

## Så här gör du:
Här är så att säga grunderna för att skriva ut debug-informatiom i Ruby.

```ruby
puts "Detta är ett meddelande"

debug = "Detta är debug information"
puts debug
```

Output:
```
Detta är ett meddelande
Detta är debug information
```

Du kan också använda `p` istället för `puts` för att få mer detaljerad info.

```ruby
p debug
```

Detta kommer att ge dig outputen: `"Detta är debug information"`

## Djupdykning
Ruby, som ursprungligen skapades 1995, har länge haft möjligheten att skrivaut debug-output, det här mönstret har nu adopterats till de flesta moderna programmeringsspråk. Ett alternativ till `puts` och `p` är att använda debugging bibliotek som `byebug` eller `pry` som innehåller kraftfulla funktioner för att felsöka din kod. `puts` och `p` skriver ut informationen till standard output (`STDOUT`), vilket vanligtvis är terminalen eller konsolen.

## Se Även
För mer information och resurser om Ruby och debugging, kolla in följande:

- Officiella Ruby API dokumnetationen för puts [här](https://ruby-doc.org/core-2.6.3/IO.html#method-i-puts).
- Officiella Ruby Dokumentationen för p [här](https://ruby-doc.org/core-2.6.3/Kernel.html#method-i-p).
- En guide i hur man använder `byebug` [här](https://edgeguides.rubyonrails.org/debugging_rails_applications.html#debugging-with-the-byebug-gem).
- En guide i hur man använder `pry` [här](https://www.rubyguides.com/2020/04/ruby-pry-tutorial/).