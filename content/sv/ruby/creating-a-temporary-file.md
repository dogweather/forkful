---
title:    "Ruby: Skapa en tillfällig fil"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Varför Skapa en Temporär Fil i Ruby

När vi utvecklar program i Ruby stöter vi ibland på behovet av att skapa en temporär fil. En temporär fil är en fil som är skapad för att användas under en kort tid och sedan raderas. Detta kan vara till nytta när vi behöver tillfälligt lagra data eller utföra vissa operationer som kräver en fil som tillfälligt behövs.

## Hur man Skapar en Temporär Fil i Ruby

Det finns flera sätt att skapa en temporär fil i Ruby, men ett enkelt sätt är att använda metoden `Tempfile.new`. Nedan finns ett kodexempel som visar hur man skapar en temporär fil och skriver innehållet "Hello World!" i den.

```Ruby
require 'tempfile'

# Skapar en temporär fil
temp_file = Tempfile.new('example')

# Skriver innehållet "Hello World!" i filen
temp_file.write('Hello World!')

# Stänger filen
temp_file.close

# Visar innehållet i filen
puts File.read(temp_file.path)
```

Detta kodexempel kommer att producera följande output:

```
Hello World!
```

När vi är klara med att använda den temporära filen kan vi enkelt radera den genom att använda metoden `temp_file.unlink`.

## Djupdykning i Skapandet av en Temporär Fil

När vi använder metoden `Tempfile.new` skapas en unik temporär fil för oss. Vi kan också ange en optional parametern `prefix` för att ange ett prefix för namnet på den temporära filen.

```Ruby
# Skapar en temporär fil med prefix "example-"
temp_file = Tempfile.new('example-')
```

Metoden `Tempfile.new` tar även emot en optional parameter `tmpdir` där vi kan specificera en annan mapp för att lagra den temporära filen istället för den standardmapp som används av systemet.

```Ruby
# Skapar en temporär fil i mappen "/home/user/temp"
temp_file = Tempfile.new('example', '/home/user/temp')
```

Det finns också flera andra metoder som vi kan använda på vår temporära fil, till exempel `temp_file.open` för att öppna filen, `temp_file.size` för att hämta storleken på filen och `temp_file.path` för att hämta sökvägen till filen.

# Se också

- [Ruby Dokumentation - Tempfile](https://ruby-doc.org/stdlib-2.7.0/libdoc/tempfile/rdoc/Tempfile.html)
- [RubyMonk - Writing to Files](https://rubymonk.com/learning/books/1-ruby-primer/chapters/1-strings/lessons/49-writing-to-files)