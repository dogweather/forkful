---
date: 2024-01-20 17:55:11.157626-07:00
description: "L\xE4sa en textfil? Det handlar om att f\xE5 fram informationen skriven\
  \ i filen och anv\xE4nda den i ditt program. Utvecklare g\xF6r det f\xF6r att ladda\
  \ data,\u2026"
lastmod: '2024-03-13T22:44:38.449137-06:00'
model: gpt-4-1106-preview
summary: "L\xE4sa en textfil? Det handlar om att f\xE5 fram informationen skriven\
  \ i filen och anv\xE4nda den i ditt program. Utvecklare g\xF6r det f\xF6r att ladda\
  \ data,\u2026"
title: "L\xE4sa en textfil"
weight: 22
---

## Vad & Varför?
Läsa en textfil? Det handlar om att få fram informationen skriven i filen och använda den i ditt program. Utvecklare gör det för att ladda data, konfigurera program eller bearbeta filer skapade av andra program.

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
