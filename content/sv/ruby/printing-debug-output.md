---
title:                "Ruby: Skriv ut felsökningsutdata"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Ibland när du programmerar i Ruby kan det hända att du stöter på buggar eller fel i din kod. För att hitta dessa fel och kunna åtgärda dem behöver du ett sätt att visa vad som händer i din kod under körningen. Det är där utskrift av debug information kommer in.

## Så här gör du

Det finns ett enkelt sätt att skriva ut debug information i Ruby genom att använda metoden `puts()`. Du kan lägga till `puts()` i olika delar av din kod för att visa information om vad som händer vid varje steg.

```Ruby
puts("Debug utskrift här")
```

Du kan också använda `p()` metoden för att skriva ut mer detaljerad information om ett specifikt objekt.

```Ruby
p(objekt)
```

Även om dessa metoder är enkla kan de vara mycket hjälpsamma när du behöver förstå vad som händer i din kod.

## Djupdykning

Det finns flera sätt att använda utskrift av debug information i Ruby. En annan metod som kan vara användbar är att använda `inspect()` metoden för att få en detaljerad lista på alla attribut och värden för ett objekt.

```Ruby
puts(objekt.inspect)
```

Du kan också använda `gets()` metoden för att få input från användaren och sedan skriva ut det för att kontrollera om värdet är korrekt.

```Ruby
input = gets()
puts("Användarens input var: #{input}")
```

En annan fördel med att använda utskrift av debug information är att det kan hjälpa dig att få en bättre förståelse för hur din kod fungerar och hjälpa dig att förbättra den.

## Se även

- [Ruby dokumentation](https://www.ruby-lang.org/sv/documentation/)
- [Debugging med pry](https://rubygems.org/gems/pry)
- [Ruby for nybörjare](https://learnrubythehardway.org/book/ex46.html)