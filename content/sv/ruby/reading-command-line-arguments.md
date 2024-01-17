---
title:                "Läsning av kommandoradsargument"
html_title:           "Ruby: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsning av kommandoradsargument är ett sätt för programmerare att ta emot inmatning från användare genom att skriva in kommandon i en terminal. Detta är användbart eftersom det gör det möjligt att skapa interaktiva program som kan anpassas efter användarens behov.

## Så här gör man:
```Ruby 
# Exempel på hur man läser kommandoradsargument
ARGV.each do|arg|
  puts "Argument: #{arg}"
end

# Användaren skriver in "ruby arguments.rb Hej världen"

# Utmatningen blir:
# Argument: Hej
# Argument: världen
```

## Gräva djupare:
Historiskt har kommandoradsargument använts främst för interaktion med datoroperativsystem, men nu används det också i många olika program och språk. Alternativ till att läsa kommandoradsargument kan vara att använda ett grafiskt gränssnitt för interaktion med användaren. Implementationen av läsning av kommandoradsargument kan variera beroende på det specifika programspråket och operativsystemet.

## Se också:
- [Ruby dokumentation om ARGV](https://ruby-doc.org/core-# . ARGV. html)
- [En tutorial om hur man läser kommandoradsargument i Ruby](https://www.rubyguides. com/ruby-# guide-TIO-cruncher-ruby-arguments/)