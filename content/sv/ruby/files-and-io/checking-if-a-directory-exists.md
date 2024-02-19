---
aliases:
- /sv/ruby/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:17.757912-07:00
description: "Att kontrollera om en katalog finns i Ruby l\xE5ter programmerare verifiera\
  \ n\xE4rvaron av en katalog innan de utf\xF6r operationer som att l\xE4sa filer\
  \ eller skapa\u2026"
lastmod: 2024-02-18 23:08:52.308915
model: gpt-4-0125-preview
summary: "Att kontrollera om en katalog finns i Ruby l\xE5ter programmerare verifiera\
  \ n\xE4rvaron av en katalog innan de utf\xF6r operationer som att l\xE4sa filer\
  \ eller skapa\u2026"
title: Kontrollera om en katalog existerar
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns i Ruby låter programmerare verifiera närvaron av en katalog innan de utför operationer som att läsa filer eller skapa nya kataloger. Detta är avgörande för att undvika fel i filhantering och för att säkerställa tillförlitligheten hos filsystemmanipulationer.

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
