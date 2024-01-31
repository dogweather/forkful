---
title:                "Een debugger gebruiken"
date:                  2024-01-28T22:08:45.932461-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een debugger gebruiken"

category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een debugger gebruiken in Ruby geeft programmeurs een superkracht om hun code te pauzeren, variabelen te inspecteren en stap voor stap door hun code te lopen. Mensen doen dit om bugs te verpletteren, de codeflow te begrijpen, en om precies te zien wat hun geschreven spreuken (code) doen wanneer de magie gebeurt - of niet.

## Hoe:

Ruby wordt geleverd met een ingebouwde debugger genaamd `byebug`. Voeg eerst `byebug` toe aan je Gemfile en voer `bundle install` uit. Plaats vervolgens `byebug` precies waar je wilt dat je programma een pauze neemt.

```Ruby
require 'byebug'

def bereken_magie(number)
  byebug
  magisch_getal = number * 7
  return magisch_getal
end

puts bereken_magie(6)
```

Het uitvoeren van dit script zal de uitvoering bij `byebug` stopzetten, en je wordt in een interactieve sessie gegooid waar je commando's kunt typen zoals:

```
step
next
continue
var local
```

Een voorbeeldoutput zou je een prompt geven die er zo uitziet:

```
[2, 11] in voorbeeld.rb
    2: 
    3: def bereken_magie(number)
    4:   byebug
=>  5:   magisch_getal = number * 7
    6:   return magisch_getal
    7: end
    8: 
    9: puts bereken_magie(6)
(byebug) 
```

## Dieper duiken:

Lang voor `byebug`, gebruikten Rubyisten `debugger` en `pry`. De laatste, `pry`, is meer dan een debugger; het is een krachtige REPL die ook gebruikt kan worden voor debugging met het breakpoint `binding.pry`.

Alternatieven voor Ruby's `byebug` zijn onder andere `pry-byebug`, dat `pry` combineert met de functionaliteit van `byebug`, en `ruby-debug`, een oudere gem die niet actief wordt onderhouden.

Wanneer je `byebug` aanroept, pauzeert de debugger je code uitvoering en geeft je een kijkje in de runtime. Je kunt variabelen zien en veranderen, naar verschillende punten in de code springen en zelfs wat Ruby-code regel voor regel uitvoeren. Het is een beetje alsof je tijdsreisvaardigheden hebt voor je Ruby-code.

## Zie ook:

- Byebug GitHub-repository: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry Documentatie: [https://github.com/pry/pry](https://github.com/pry/pry)
- Een gids voor het debuggen van Rails Apps: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
