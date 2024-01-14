---
title:    "Ruby: Läsning av kommandoradsargument"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

##Varför

Command line-argument är en grundläggande del av programmering, och kunna läsa och använda dem effektivt är en viktig färdighet för en Ruby-utvecklare. Att förstå hur man läser och använder kommandoradsargument kan hjälpa till att skapa mer flexibla och anpassningsbara program.

##Så här gör du

Att läsa kommandoradsargument i Ruby är enkel och kan göras med hjälp av ARGV metoden. Detta är en inbyggd metod som återger en array av argument som skickats till programmet via kommandoraden. Här är ett enkelt exempel för att illustrera hur man använder den:

```Ruby
#exempel.rb

puts "Välkommen till min blogg!"

puts "Vad heter du?"
name = ARGV[0]
puts "Hejsan, #{name}!"
```

I detta exempel ber vi användaren att lägga till sitt namn som ett argument när de kör programmet i kommandoraden:

```bash
ruby exempel.rb Lisa
Välkommen till min blogg!
Vad heter du?
Hejsan, Lisa!
```

Som du kan se, läser vi namnet som skickats in via kommandoraden genom att använda indexet [0] på ARGV-metoden.

##Djupdykning

Vidare kan vi utföra olika operationer på ARGV-metoden för att anpassa den till våra behov. Till exempel kan vi använda ARGV.length för att ta reda på antalet argument som skickats in eller ARGV.join(',') för att omvandla argumenten till en kommaseparerad sträng.

Vi kan också använda optparse biblioteket för att skapa en mer avancerad strukturerad läsning av kommandoradsargument. Detta bibliotek hjälper till att hantera argument av enklare format och kan ge användbara felmeddelanden om argument som inte är giltiga.

##Se också

- [Ruby ARGV dokumentation](https://ruby-doc.org/core-3.0.1/ARGF.html)
- [ruby-doc.org ARGV artiklar](https://ruby-doc.org/articles/command_line.html)
- [Ruby getoptlong bibliotek](https://ruby-doc.org/stdlib-3.0.1/libdoc/getoptlong/rdoc/GetoptLong.html)