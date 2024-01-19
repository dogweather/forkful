---
title:                "Kontrollera om en katalog finns"
html_title:           "C: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontrollera om en katalog existerar i Ruby är en handling av att verifiera om en specifik katalog redan finns i filsystemet. Programmerare gör detta för att undvika fel vid försök att läsa, skriva eller ändra kataloger som inte existerar.

## Hur man:

Du kan kontrollera om en katalog existerar i Ruby med `Dir.exist?` metoden. Här är ett exempel:

```Ruby 
if Dir.exist?('/path/to/directory')
  puts "Katalogen finns!"
else
  puts "Katalogen finns inte!"
end
```

Om katalogen finns skrivs meddelandet "Katalogen finns!" ut, annars skrivs "Katalogen finns inte!".

## Djupt Dyk:

1. Historisk kontext: `Dir.exist?` metoden har funnits sedan Ruby version 1.9.3 och det är den rekommenderade metoden för att kolla om en katalog existerar eftersom den inte väcker någon undantag.
   
2. Alternativ: Ett annat sätt att kontrollera om en katalog existerar är med `File.directory?`metoden. Men, `Dir.exist?` har fördelen att det är mer läsbart och tydligt.

3. Implementeringsdetaljer: `Dir.exist?` utför en systemanrop till filsystemet för att kolla om katalogen existerar vilket kan vara latency-känsligt i vissa fall.

## Se även:

1. Officiell Ruby dokumentation för Dir.exits?: [https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F) 
2. Stack Overflow tråd om skillnader mellan `Dir.exist?` och `File.directory?`: [https://stackoverflow.com/questions/15266730/difference-between-file-directory-and-dir-exist](https://stackoverflow.com/questions/15266730/difference-between-file-directory-and-dir-exist) 
3. Blog post om Ruby fil- och kataloghantering: [http://zetcode.com/lang/rubytutorial/io/](http://zetcode.com/lang/rubytutorial/io/)