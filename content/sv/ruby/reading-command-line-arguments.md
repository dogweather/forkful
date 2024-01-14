---
title:    "Ruby: Läsning av kommandoradsargument"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en viktig del av programmering i Ruby eftersom det ger användaren möjlighet att interagera med programmet genom att skicka in olika värden vid körning.

## Hur man gör det

För att läsa kommandoradsargument i Ruby kan man använda ARGV-arrayen. Den här arrayen innehåller alla argument som skickas in via kommandoraden, där det första argumentet ligger på index 0 och de följande argumenten på efterföljande index.

För att kunna använda ARGV-arrayen behöver vi först skapa en variabel som tar emot argumenten, till exempel "args". Sedan kan vi använda metoden "each" för att loopa igenom alla argument och utföra en handling på dem.

I kodexemplet nedan visar jag hur man kan skriva ett program som tar in två tal som kommandoradsargument och sedan adderar dem och skriver ut resultatet.

```Ruby
args = ARGV
tal1 = args[0].to_i
tal2 = args[1].to_i
summa = tal1 + tal2

puts "Summan av #{tal1} och #{tal2} är #{summa}"
```

Om vi kör programmet med kommandoradsargumenten "ruby program.rb 2 5" kommer vi få utskriften "Summan av 2 och 5 är 7".

## Djupdykning

Utöver att läsa in kommandoradsargument finns det även andra sätt att interagera med användaren genom kommandoraden i Ruby. Till exempel kan man använda sig av gemet "tty-prompt" för att skapa en enklare och mer användarvänlig interaktion.

En annan intressant aspekt är att ARGV-arrayen även innehåller argumentet "-e" vilket gör det möjligt att skicka in en enstaka Ruby-rad som argument och få den exekverad.

## Se även

- [Ruby ARGV documentation](https://ruby-doc.org/core-2.7.0/ARGF.html)
- [TTY-prompt gem](https://github.com/piotrmurach/tty-prompt)
- [Ruby command line arguments tutorial](https://www.rubyguides.com/2012/02/ruby-command-line-args/)