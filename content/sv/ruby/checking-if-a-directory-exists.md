---
title:                "Ruby: Kontrollera om en mapp existerar"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Varför
Att kontrollera om en katalog existerar är en viktig del av Ruby-programmering. Det hjälper dig att undvika fel och förhindra att din kod kraschar om filer eller kataloger inte finns.

##Hur man gör
Det finns flera olika sätt att kontrollera om en katalog existerar i Ruby. Ett sätt är att använda File.exist?() metoden. Här är ett exempel på hur du kan använda den:

```Ruby
if File.exist?(“/hemanvandare/dokumenter”)
  puts “Katalogen existerar!”
else
  puts “Katalogen finns inte.”
end
```

I detta exempel använder vi File.exist?() metoden för att kolla om katalogen "dokumenter" existerar i mappen "hemanvandare". Om den gör det, skrivs ett meddelande ut som säger att katalogen existerar. Om den inte gör det, skrivs ett annat meddelande ut.

För att kontrollera om en katalog existerar i en annan mapp, kan du ändra sökvägen i File.exist?() metoden. Till exempel, om du vill kontrollera om en katalog existerar i "desktop" mappen, kan du använda "/hemanvandare/desktop/katalogen".

##Djupdykning
Det finns också andra sätt att kontrollera om en katalog existerar i Ruby. En annan metod är Dir.exist?(), som gör exakt samma sak som File.exist?() metoden, men den är speciellt för kataloger. Här är ett exempel:

```Ruby
if Dir.exist?(“/hemanvandare/dokumenter”)
  puts “Katalogen existerar!”
else
  puts “Katalogen finns inte.”
end
```

Det finns också möjlighet att använda Dir.glob() metoden för att kontrollera om en katalog existerar. Denna metod används för att hitta filer eller mappar som matchar ett visst mönster. Om Du vill läsa mer om dessa metoder kan du använda Ruby-dokumentationen för att få mer information och fler exempel.

##Se även
- Ruby-dokumentationen för File, Dir, och Glob klasserna:
  - https://ruby-doc.org/core-3.0.0/File.html
  - https://ruby-doc.org/core-3.0.0/Dir.html
  - https://ruby-doc.org/core-3.0.0/Dir.html#method-c-glob