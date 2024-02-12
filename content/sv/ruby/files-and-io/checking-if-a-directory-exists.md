---
title:                "Kontrollera om en katalog existerar"
aliases: - /sv/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:17.757912-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
