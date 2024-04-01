---
date: 2024-01-26 04:09:53.031444-07:00
description: "Att anv\xE4nda en debugger i Ruby ger programmerare en superkraft att\
  \ pausa sin kod, inspektera variabler och stega igenom sin kod rad f\xF6r rad. Folk\
  \ g\xF6r det\u2026"
lastmod: '2024-03-13T22:44:38.435467-06:00'
model: gpt-4-0125-preview
summary: "Att anv\xE4nda en debugger i Ruby ger programmerare en superkraft att pausa\
  \ sin kod, inspektera variabler och stega igenom sin kod rad f\xF6r rad. Folk g\xF6\
  r det\u2026"
title: "Att anv\xE4nda en debugger"
---

## Hur man gör:
Ruby kommer med en inbyggd debugger som heter `byebug`. Först, inkludera `byebug` i din Gemfile och kör `bundle install`. Sedan, placera `byebug` precis där du vill att ditt program ska ta en paus.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magiskt_nummer = number * 7
  return magiskt_nummer
end

puts calculate_magic(6)
```

Att köra detta skript kommer att stoppa exekveringen vid `byebug`, och du kommer att kastas in i en interaktiv session där du kan skriva kommandon som:

```
step
next
continue
var local
```

Exempelutdata skulle ge dig en uppmaning som ser ut så här:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magiskt_nummer = number * 7
    6:   return magiskt_nummer
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Fördjupning:
Långt tillbaka före `byebug`, använde Rubyister `debugger` och `pry`. Den senare, `pry`, är mer än en debugger; det är en kraftfull REPL som också kan användas för debugging med brytpunkten `binding.pry`.

Alternativ till Rubys `byebug` inkluderar `pry-byebug`, som kombinerar `pry` med `byebug`-funktionalitet, och `ruby-debug`, som är en äldre juvel som inte aktivt underhålls.

När du anropar `byebug`, pausar debuggern din kodexekvering och ger dig en titt på körningen. Du kan se och ändra variabler, hoppa till olika punkter i koden och till och med köra lite Ruby-kod rad för rad. Det är lite som att ha tidsreseförmågor för din Ruby-kod.

## Se också:
- Byebug GitHub Repository: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry Dokumentation: [https://github.com/pry/pry](https://github.com/pry/pry)
- En guide till att debugga Rails-appar: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
