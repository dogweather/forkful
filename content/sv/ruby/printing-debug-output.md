---
title:    "Ruby: Utskrift av felsökningsutmatning"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva program är en komplex process som kan innehålla många fel och buggar. Att använda sig av debug output, eller utmatning av felsökningsinformation, kan hjälpa dig att identifiera och lösa problem i ditt Ruby-program. Det är ett sätt att få en bättre förståelse för vad som händer i koden och varför, vilket är avgörande för att skapa ett väl fungerande program.

## Hur man gör det

För att skriva ut debug output i ditt Ruby-program, använd dig av puts metoden. Den tar in en parameter och skriver ut den till terminalen. En vanlig teknik är att skriva ut variabler eller delar av koden för att se värdet eller stegen den går igenom. Här nedan följer ett exempel:

```Ruby
text = "Hej världen"
puts text
```

Detta kommer att skriva ut "Hej världen" i terminalen. Om du vill skriva ut värdet på en variabel, till exempel en array, kan du göra det så här:

```Ruby
numbers = [1, 2, 3, 4]
puts numbers.inspect
```

Detta kommer att skriva ut hela arrayen med dess innehåll. Du kan också skriva ut flera variabler på samma rad genom att använda en komma mellan dem.

```Ruby
name = "Lisa"
age = 27
puts "Namn: #{name}, Ålder: #{age}"
```

Detta kommer att skriva ut "Namn: Lisa, Ålder: 27". Genom att använda interpolation (#{}), kan du inkludera variabler i en sträng för att skriva ut deras värden.

## Djupdykning

Förutom att skriva ut variabler och värden, kan du även använda dig av andra metoder för att få mer detaljerad debug output. En vanlig metod är att använda sig av "puts messages" för att skriva ut meddelanden i olika delar av koden för att följa dess exekvering. Du kan också använda dig av "binding.pry" för att få programmet att pausa vid en viss del av koden och sedan undersöka variabler och göra ändringar i realtid.

En annan användbar metod är ".inspect" eller ".to_s" som kan användas för att få ut mer detaljerad information om variabler och objekt. Detta kan vara särskilt användbart när du arbetar med objekt av olika klasser där standardutskriften inte ger tillräckligt med information.

## Se även

Här är några användbara resurser för att lära sig mer om debugging i Ruby:

- [Ruby on Rails Guides - Debugging](https://guides.rubyonrails.org/debugging_rails_applications.html)
- [Pry gem official documentation](https://github.com/pry/pry)
- [Bloggpost från Thoughtbot - "Debugging like a boss with Pry"](https://thoughtbot.com/blog/debugging-like-a-boss-with-pry)
- [Ruby's official documentation for built-in classes and modules](https://ruby-doc.org/core-2.6/)

Genom att använda dig av debug output kan du effektivt felsöka ditt Ruby-program och skapa ett bättre fungerande program. Med olika tekniker och verktyg tillgängliga, kan du enkelt spåra fel och förbättra din kod. Ta dig tid att lära dig mer om debugging för att bli en mer effektiv Ruby-programmerare.