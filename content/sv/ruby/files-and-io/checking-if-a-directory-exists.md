---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:17.757912-07:00
description: "Hur g\xF6r man: Rubys standardbibliotek erbjuder enkla metoder f\xF6\
  r att kontrollera om en katalog finns. S\xE5 h\xE4r g\xF6r du det med ren Ruby,\
  \ utan att beh\xF6va n\xE5gra\u2026"
lastmod: '2024-03-13T22:44:38.446035-06:00'
model: gpt-4-0125-preview
summary: "Rubys standardbibliotek erbjuder enkla metoder f\xF6r att kontrollera om\
  \ en katalog finns."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur gör man:
Rubys standardbibliotek erbjuder enkla metoder för att kontrollera om en katalog finns. Så här gör du det med ren Ruby, utan att behöva några tredjepartsbibliotek:

```ruby
require 'fileutils'

# Kontrollera om en katalog finns
if Dir.exist?('/sökväg/till/katalog')
  puts 'Katalogen finns.'
else
  puts 'Katalogen finns inte.'
end
```
Exempel på utskrift:
```
Katalogen finns.
```
Eller:
```
Katalogen finns inte.
```

Förutom att använda `Dir.exist?`, kan du också använda metoden `File.directory?` som returnerar `true` om den angivna sökvägen är en katalog:

```ruby
if File.directory?('/sökväg/till/katalog')
  puts 'Katalogen finns.'
else
  puts 'Katalogen finns inte.'
end
```
Både `Dir.exist?` och `File.directory?` är en del av Rubys standardbibliotek och kräver inte några externa gems att använda, vilket gör dem till bekväma och effektiva alternativ för katalogkontroller.
