---
title:                "Att skapa en temporär fil"
html_title:           "Ruby: Att skapa en temporär fil"
simple_title:         "Att skapa en temporär fil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa temporära filer är ett vanligt förfarande inom programmering, särskilt inom Ruby. Dessa filer används för att temporärt lagra data eller utföra vissa operationer innan de raderas. Det kan vara användbart för att göra temporära backupfiler eller för att hantera stora datamängder som behöver rensas upp efteråt.

## Så här gör du

För att skapa en temporär fil i Ruby, använder du "Tempfile" klassen. Här är ett exempel på kod som skapar en temporär fil och skriver några rader till den:

```Ruby
require 'tempfile'

temp_file = Tempfile.new('example') # "example" är det prefix som används för filnamnet
temp_file.puts "Detta är en temporär fil."
temp_file.puts "Den kommer att raderas när programmet avslutas."
```

Om du vill ha åtkomst till den temporära filen under programmets körning, kan du använda "path" metoden för att få filvägen:

```Ruby
puts temp_file.path #=> /tmp/example20190201-64362-2ais0h
```

För att radera den temporära filen efter att du är klar med den, kan du använda "delete" metoden:

```Ruby
temp_file.delete # raderar filen här
```

## Djupdykning

När du använder "Tempfile" klassen, skapas faktiskt inte en riktig fil på din dator. Istället skapas en temporär fil i ditt systems "temp" mapp. Detta är en förinställd plats där temporära filer lagras och som töms av systemet regelbundet.

Du kan också ange en anpassad mapp för att lagra dina temporära filer genom att använda "tmpdir" argumentet:

```Ruby
Tempfile.new('example', '/Users/johndoe/Documents') #=> /Users/johndoe/Documents/example20190201-64362-2ais0h
```

En annan viktig sak att notera är att den temporära filen kommer att raderas automatiskt när programmet avslutas, men om du vill kan du också specificera att filen ska raderas direkt genom att använda "close" metoden:

```Ruby
temp_file.close # stänger och raderar filen direkt
```

## Se även

- [Ruby Tempfile dokumentation](https://ruby-doc.org/stdlib-2.6.1/libdoc/tempfile/rdoc/Tempfile.html)
- [Ruby Tempfile blogginlägg av David Black](http://davidbliss.com/2007/03/02/advanced-temp-file-management-in-ruby/)
- [The Ruby Toolbox - Tempfiles](https://www.ruby-toolbox.com/categories/tempfiles)