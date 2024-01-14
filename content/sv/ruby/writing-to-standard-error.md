---
title:    "Ruby: Skrivande till standardfel"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Varför

Att skriva till standard error är ett användbart verktyg för att felsöka dina Ruby-program. Genom att skriva till standard error kan du skriva ut specifika meddelanden som hjälper dig att identifiera och förstå varför ett program inte fungerar som det ska.

# Hur man gör det

För att skriva till standard error i Ruby, används metoden `warn`. Detta gör att meddelanden skrivs ut till standard error istället för till standard output. Här är ett exempel:

```Ruby
def divide(x, y)
  if y == 0
    warn "Kan inte dividera med 0!"
  else
    puts x / y
  end
end

divide(10, 0)
```

Output:

```sh
Kan inte dividera med 0!
```

I detta exempel använder vi `warn` för att skriva ut ett felmeddelande om vi försöker dividera med 0. Detta hjälper oss att snabbt identifiera felet i vårt program.

# Djupdykning

I vissa fall kan du vilja skriva till standard error även när allt fungerar som det ska. Detta kan vara till nytta när du vill skriva ut debugging-meddelanden för att kontrollera variabler eller andra värden i ditt program.

En annan användbar metod är `raise`, som låter dig kasta ett undantag och skriva ett meddelande till standard error. Detta är användbart för att fånga och hantera fel i ditt program.

```Ruby
def get_user_age
  age = gets.chomp.to_i
  if age < 18
    raise "Användaren är inte myndig!"
  else
    puts "Användaren är ${age} år gammal."
  end
end

get_user_age()
```

Output:

```sh
Användaren är inte myndig!
```

## Se även

Här är några resurser du kan använda för att lära dig mer om att skriva till standard error i Ruby:

- Ruby dokumentation: https://ruby-doc.org/core-3.0.0/Kernel.html#method-i-warn
- Learn Ruby the Hard Way: https://learnrubythehardway.org/book/ex17.html
- RubyGuides: https://www.rubyguides.com/2019/02/ruby-stderr-stdout/