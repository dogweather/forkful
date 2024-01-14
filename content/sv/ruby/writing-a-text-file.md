---
title:    "Ruby: Skriva en textfil"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att skriva en textfil är ett viktigt steg i många Ruby-programmeringsprojekt. Det kan hjälpa till att spara data eller skapa en användarvänlig sätt att interagera med ditt program.

## Hur man gör
För att skapa en textfil i Ruby, börja med att öppna en ny fil med ".rb" filändelsen. Använd sedan ```File.open``` metoden för att skapa och öppna en ny textfil. Du kan ge filen ett namn och välja vilket läge den ska öppnas i (skrivning, läsning eller båda).

```
File.open("mitt_program.txt", "w") do |file|
  file.puts "Hej! Det här är mitt första Ruby-program."
  file.puts "Det är så kul att lära sig ett nytt programmeringsspråk!"
end
# Skapar en fil med namnet "mitt_program.txt" och lägger till två rader av text i den.
```

## Djupdykning
Det finns flera olika sätt att skriva en textfil i Ruby, inklusive att använda ```puts``` metoden eller ```IO.write``` metoden. Men att använda ```File.open``` metoden ger dig mer kontroll och möjlighet att hantera eventuella fel som kan uppstå.

Du kan också använda ```.close``` metoden för att stänga filen när du är klar med att skriva till den, vilket frigör ramminne och förhindrar eventuella problem med andra program som kan försöka öppna filen samtidigt.

## Se också
- [Official Ruby Documentation on File I/O](https://ruby-doc.org/core-2.6.3/File.html)
- [Codecademy's Learn Ruby Course](https://www.codecademy.com/learn/learn-ruby)
- [Ruby on Rails Tutorial](https://www.railstutorial.org/)