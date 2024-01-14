---
title:    "Ruby: Skapa en tillfällig fil"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Varför skapa en temporär fil i Ruby?

Att skapa temporära filer är en vanlig uppgift inom programmering och kan ha många olika användningsområden. I Ruby kan detta vara speciellt användbart när du behöver hantera data eller filer på ett tillfälligt sätt, till exempel vid testning eller filöverföringar.

## Så här skapar du en temporär fil i Ruby

Det finns flera sätt att skapa en temporär fil i Ruby, men en av de vanligaste är att använda File-klassen och dess metod `open()`. Du kan ange ett prefix för filnamnet och en mapp där filen ska skapas, eller så skapas filen automatiskt i den temporära mappen för ditt operativsystem.

```ruby
temp_file = File.open("prefix_", "w", tmpdir: "tmp/folder")
puts temp_file.path
```

Detta skapar en temporär fil med namnet "prefix_randomcharacters" i mappen "tmp/folder". Om du inte anger ett prefix så kommer filen att få ett automatiskt genererat namn.

## Djupdykning i skapandet av temporära filer

När du har skapat en temporär fil i Ruby, kan du utföra olika åtgärder på den, som att skriva eller läsa data. Det är viktigt att komma ihåg att en temporär fil automatiskt kommer att raderas när ditt program avslutas eller när du stänger filen. Om du behöver använda innehållet i filen efter stängningen, kan du antingen flytta filen till en annan plats eller läsa innehållet i en variabel innan du stänger filen.

En annan användbar funktion är att kunna ändra filens läge till "skrivning" eller "läsning" beroende på dina behov. Detta kan göras genom att använda metoderna `close_write` och `close_read`.

```ruby
temp_file = File.open("prefix_", "w+", tmpdir: "tmp/folder")
temp_file.write("This is a sample text.")
temp_file.close_write
```

I exemplet ovan skapar vi en temporär fil för både skrivning och läsning, skriver en text i filen och stänger sedan läget för skrivning. Detta gör att vi fortfarande kan läsa filen men inte längre ändra på den.

## Se även

- [Ruby dokumentation för File-klassen][1]
- [En guide för hantering av temporära filer i Ruby][2]
- [En tutorial för att skapa och använda temporära filer i Ruby][3]

[1]: https://ruby-doc.org/core/File.html
[2]: https://www.rubyguides.com/2015/02/ruby-temporary-files/
[3]: https://medium.com/@andrubyista/managing-temporary-files-in-ruby-3578ca8ebd36