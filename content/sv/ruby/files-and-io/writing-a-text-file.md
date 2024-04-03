---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:01.405081-07:00
description: "Hur man g\xF6r: Ruby g\xF6r filoperationer enkla. F\xF6r att skriva\
  \ till en fil kan du anv\xE4nda Rubys inbyggda `File`-klass. F\xF6ljande exempel\
  \ demonstrerar hur man\u2026"
lastmod: '2024-03-13T22:44:38.450138-06:00'
model: gpt-4-0125-preview
summary: "Ruby g\xF6r filoperationer enkla."
title: Att skriva en textfil
weight: 24
---

## Hur man gör:
Ruby gör filoperationer enkla. För att skriva till en fil kan du använda Rubys inbyggda `File`-klass. Följande exempel demonstrerar hur man öppnar en fil för att skriva (`"w"`-läge) och lägga till (`"a"`-läge), sedan skriver en sträng till den och säkerställer att filen är stängd efteråt:

```ruby
# Skriver nytt innehåll till en fil, skriver över befintligt innehåll
File.open("example.txt", "w") do |file|
  file.puts "Hej, Ruby!"
end

# Lägger till innehåll i slutet av en fil
File.open("example.txt", "a") do |file|
  file.puts "Lägger till en annan rad."
end
```
Efter att ha kört båda kodsnuttena kommer innehållet i `example.txt` att vara:
```
Hej, Ruby!
Lägger till en annan rad.
```

### Använda ett tredjepartsbibliotek: FileUtils
För mer komplexa filoperationer kan Ruby standardbibliotek `FileUtils` vara till stor hjälp, även om för grundläggande filskrivning är standard `File`-metoder tillräckliga. Men, om du vill kopiera, flytta, ta bort eller utföra andra filsystemoperationer i samband med filskrivning, så är `FileUtils` värt att utforska.

Ett exempel på att använda `FileUtils` för att skapa en katalog och sedan skriva till en fil inom den katalogen:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/idag.log", "w") do |file|
  file.puts "Loggpost: #{Time.now}"
end
```

Detta demonstrerar skapandet av en ny katalog `logs` om den inte redan finns, och att skriva till en ny fil `idag.log` inom den, vilket visar både hantering av kataloger och filer utan att direkt skriva med FileUtils, men utnyttjar dess förmåga att hantera kataloger.
