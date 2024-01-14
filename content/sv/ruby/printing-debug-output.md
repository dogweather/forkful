---
title:    "Ruby: Utskrift av felsökningsutdata"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför
Att skriva programmeringskod är en process som kan bli komplex och ibland frustrerande. Oavsett hur erfaren du är som programmerare, kommer du ibland stöta på problem som inte är uppenbara vid första anblicken. Då kan det vara till stor hjälp att använda sig av debug-utskrifter för att förstå vad som händer i koden i realtid. Detta gör det möjligt att hitta och åtgärda fel och förbättra programmet.

## Hur man gör det
För att skriva ut debug-utskrifter i Ruby, använd `puts`kommandot. Detta kommer att skriva ut ett värde eller en sträng i terminalen, vilket hjälper dig att spåra vad som händer i koden. Du kan också använda `p`kommandot, som skriver ut värdet av ett uttryck och lägger till en ny rad i slutet. Detta kan vara användbart för att se värdet av variabler i en loop eller funktion.

```Ruby
num = 5
puts "Värdet på num är #{num}"
p "Värdet på num är #{num}"
```

Output:

```Ruby
Värdet på num är 5
"Värdet på num är 5\n"
```

## Djupdykning
Om du vill ha mer detaljerad information eller mer kontroll över hur dina debug-utskrifter ser ut, kan du använda `pp` kommandot för Pretty Print. Detta kommer att formatera din output på ett snyggt och lättläsligt sätt. Du kan också använda villkorliga uttryck för att bara skriva ut uttryck om vissa villkor är uppfyllda.

```Ruby
num = 10
pp "Detta är ett jämnt tal" if num % 2 == 0
pp "Detta är ett udda tal" if num.odd?
```

Output:

```Ruby
"Detta är ett udda tal"
```

## Se även
- [The Ruby debugging guide](https://www.rubyguides.com/2019/02/ruby-debugging/)
- [Using puts, p and print in Ruby](https://mixandgo.com/blog/how-to-use-puts-p-print-in-ruby)
- [Debugging in Ruby: A thorough guide](https://www.learnenough.com/debugging-tutorial/how_to_debug)