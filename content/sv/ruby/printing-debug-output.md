---
title:                "Utmatning av felsökningsresultat"
html_title:           "Ruby: Utmatning av felsökningsresultat"
simple_title:         "Utmatning av felsökningsresultat"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod kan vara rörigt och ibland är det lätt att komma bort sig. Genom att skriva ut debuggmeddelanden kan du enkelt spåra vad som händer i koden och hitta och åtgärda eventuella fel. Det är ett viktigt verktyg för att förbättra din kod och göra den mer robust.

## Så här gör du

För att skriva ut debuggmeddelanden i Ruby, använder du metoden `puts` och skriver ut det du vill undersöka. Till exempel:

```Ruby
puts "Hello World!"
```
Detta kommer att skriva ut "Hello World!" i konsolen när du kör ditt program. Du kan också skriva ut variabler eller uttryck för att se deras värde. Till exempel:

```Ruby
x = 5
puts x
```
Detta kommer att skriva ut värdet 5 till konsolen. Det kan vara användbart när du vill följa värdet på en variabel genom koden.

## Djupdykning

När du skriver ut debuggmeddelanden är det viktigt att vara noggrann och bara skriva ut relevanta saker. Att skriva ut för mycket kan göra det svårt att hitta det du letar efter. Det kan också påverka prestandan i ditt program.

Istället för att skriva ut samma meddelande flera gånger kan du använda placeholders och interpolering för att visa variabler och uttryck. Till exempel:

```Ruby
name = "Lisa"
puts "Hello #{name}!"
```
Detta kommer att skriva ut "Hello Lisa!" och ger dig möjlighet att använda variabler i dina meddelanden.

Det finns också andra metoder som `p` och `pp` som kan skriva ut mer detaljerad information om objekt och hjälpa dig att felsöka mer komplexa problem.

## Se också

- [Ruby's officiella dokumentation om debugging](https://ruby-doc.org/core-2.6.5/Kernel.html#method-i-p)
- [En guide till Ruby's debugger](https://www.rubyguides.com/2017/07/ruby-debugger/)
- [Tips och tricks för debugging i Ruby](https://www.rubylangugaejournal.com/blog/how-to-debug-ruby-code/)