---
date: 2024-01-20 17:55:11.157626-07:00
description: "How to: Anv\xE4nd `File`-klassen f\xF6r att l\xE4sa filer. H\xE4r \xE4\
  r enkla exempel."
lastmod: '2024-03-13T22:44:38.449137-06:00'
model: gpt-4-1106-preview
summary: "Anv\xE4nd `File`-klassen f\xF6r att l\xE4sa filer."
title: "L\xE4sa en textfil"
weight: 22
---

## How to:
Använd `File`-klassen för att läsa filer. Här är enkla exempel:

```Ruby
# Läs hela filen på en gång
innehall = File.read('exempel.txt')
puts innehall

# Läs filen rad för rad
File.foreach('exempel.txt') do |rad|
  puts rad
end
```

Förväntad utdata för båda kan vara:

```
Detta är den första raden i filen.
Och här är den andra raden.
```

## Deep Dive
Att läsa filer i Ruby är rakt på sak tack vare den inbyggda `File`-klassen, vilken har funnits sedan Rubys barndom. Alternativt kan du använda lägre nivå I/O-klasser som `IO` för specifika behov. Att läsa filen `readlines` kan vara hjälpsamt för små filer, men för större filer är `foreach` eller öppna filen med `File.open` och processa den bit för bit mer minneseffektivt. Öppna filer bör alltid stängas, antingen manuellt eller genom att använda ett block med `File.open` vilket stänger filen automatiskt efter användning.

## See Also:
Mer detaljerad information och exempel finns i Ruby-dokumentationen:
- IO-klass: https://ruby-doc.org/core/IO.html
- File-klass: https://ruby-doc.org/core/File.html
- Läsa filer effektivt: https://www.rubyguides.com/2015/05/working-with-files-ruby/
