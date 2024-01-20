---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil är att utvinna data från en fil stående på hårddisken med syftet att behandla den eller visa den på skärmen. Programmerarna gör detta för att röra om data mellan applikationer och för att lagra och hämta användarinformation till och från databaser.

## Så här gör du:
För att läsa en textfil i Ruby, använd `File` klassen.

```Ruby
file = File.open("textfile.txt")
content = file.read
puts content
file.close
```

Detta kommer att visa innehåll i filen `textfile.txt`. Kom ihåg att alltid stänga filen efter att ha läst den.

```Ruby
# Läser en fil rad för rad
File.open("textfile.txt").each do |line|
  puts line
end
```

Detta kommer att skriva ut filen rad för rad.

## Djupdykning
Metoden att läsa filer i Ruby går tillbaka till de tidiga dagarna av programmeringsspråket. Ruby erbjuder flera sätt att läsa filer för att ge flexibilitet.

Alternativen till `File.open` inkluderar `IO.read` och `IO.foreach`, som också kan användas för att läsa en fil i Ruby. `IO.read` läser hela filen på en gång, medan `IO.foreach` läser filen rad för rad.

Implementationen av filavläsning i Ruby är ganska effektiv och erbjuder ett antal alternativ. När du läser en fil får du tillgång till strömmen med information i filen. Du kan bestämma hur mycket data du vill läsa åt gången, eller du kan läsa hela filen på en gång.

## Se även
Lär dig mer om att arbeta med filer i Ruby från dessa källor:

[RubyDoc: Class File](https://ruby-doc.org/core-2.7.0/File.html)

[RubyGuides: Reading Files in Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)

[Ruby Tutorial: Ruby File Handling](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)