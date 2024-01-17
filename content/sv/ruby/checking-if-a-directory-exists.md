---
title:                "Kontrollera om en katalog finns"
html_title:           "Ruby: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att kontrollera om en katalog finns är en viktig del av programmering som hjälper till att säkerställa att programmet fungerar korrekt och undvika eventuella felmeddelanden. Det är särskilt användbart när man ska öppna eller skapa filer inom en specifik katalog.

## Hur man gör det: 
Det finns två sätt att kontrollera om en katalog finns i Ruby. Antingen kan man använda sig av standardbiblioteket 'File' eller den inbyggda metoden 'Dir.exist?'. Här är ett exempel på hur man kan använda dessa metoder:

```Ruby 
require 'file'
if File.directory?("namn på katalog")
  puts "Katalogen finns"
else
  puts "Katalogen finns inte"
end

if Dir.exist?("namn på katalog")
  puts "Katalogen finns"
else
  puts "Katalogen finns inte"
end
```

Exempeloutput: 
Katalogen finns

## Djupdykning:
När det kommer till att kontrollera om en katalog finns är det värt att nämna att 'File' biblioteket också har flera metoder som kan användas för att kontrollera om en fil eller en symbolisk länk existerar i en katalog. Några alternativ till dessa metoder är 'File.exist?' och 'File.symlink?'. Det är också värt att notera att 'Dir.exist?' returnerar en boolean (true eller false), medan 'File.directory?' returnerar katalogens namn om den finns och 'nil' om den inte existerar. Om du är osäker på vilken metod som passar dina behov bäst, kan du alltid kolla dokumentationen för att få mer information och hitta alternativ som fungerar bäst för ditt specifika projekt.

## Se också:
- [Ruby dokumentation för File](https://ruby-doc.org/core-2.7.1/File.html)
- [Ruby dokumentation för Dir](https://ruby-doc.org/core-2.7.1/Dir.html)
- [En bra guide för att använda File och Dir](https://www.rubyguides.com/2015/05/working-with-files-ruby/)