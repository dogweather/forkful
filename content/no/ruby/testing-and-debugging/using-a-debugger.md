---
date: 2024-01-26 04:09:45.451144-07:00
description: "\xC5 bruke en debugger i Ruby gir programmerere en superkraft til \xE5\
  \ pause koden sin, inspisere variabler, og g\xE5 gjennom koden sin linje for linje.\
  \ Folk gj\xF8r\u2026"
lastmod: 2024-02-19 22:05:00.605597
model: gpt-4-0125-preview
summary: "\xC5 bruke en debugger i Ruby gir programmerere en superkraft til \xE5 pause\
  \ koden sin, inspisere variabler, og g\xE5 gjennom koden sin linje for linje. Folk\
  \ gj\xF8r\u2026"
title: "\xC5 bruke en debugger"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å bruke en debugger i Ruby gir programmerere en superkraft til å pause koden sin, inspisere variabler, og gå gjennom koden sin linje for linje. Folk gjør det for å knuse feil, forstå kodeflyten, og for å se nøyaktig hva deres skrevne trylleformler (kode) gjør når magien skjer – eller ikke.

## Hvordan:

Ruby kommer med en innebygd debugger kalt `byebug`. Først, inkluder `byebug` i din Gemfile og kjør `bundle install`. Deretter, sleng inn `byebug` akkurat der du ønsker at programmet ditt skal ta en pause.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

Å kjøre dette skriptet vil stoppe utføringen ved `byebug`, og du vil bli kastet inn i en interaktiv økt hvor du kan skrive kommandoer som:

```
step
next
continue
var local
```

Eksempel på utdata vil gi deg en ledetekst som ser slik ut:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Dykk dypere:

Lenge før `byebug`, brukte Rubyister `debugger` og `pry`. Sistnevnte, `pry`, er mer enn en debugger; det er en kraftig REPL som også kan brukes til feilsøking med `binding.pry` stoppunkt.

Alternativer til Ruby's `byebug` inkluderer `pry-byebug`, som kombinerer `pry` med `byebug`-funksjonalitet, og `ruby-debug`, som er en eldre gem som ikke aktivt vedlikeholdes.

Når du aktiverer `byebug`, suspenderer debuggeren utførelsen av koden din og gir deg et innblikk i kjøretiden. Du kan se og endre variabler, hoppe til forskjellige punkter i koden, og til og med kjøre noe Ruby-kode linje for linje. Det er litt som å ha tidsreiseevner for Ruby-koden din.

## Se også:

- Byebug GitHub Repository: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry Dokumentasjon: [https://github.com/pry/pry](https://github.com/pry/pry)
- En Guide til Debugging Rails Apps: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
