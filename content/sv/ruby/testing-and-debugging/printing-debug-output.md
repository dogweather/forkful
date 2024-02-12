---
title:                "Skriva ut felsökningsdata"
aliases: - /sv/ruby/printing-debug-output.md
date:                  2024-01-20T17:53:32.788516-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Utskrift för felsökning är när vi spottar ut text till konsolen för att se vad som händer i vår kod. Programmerare gör detta för att snabbt hitta och rätta till fel.

## How to:
Ruby ger oss `puts` och `p` för att skriva ut värden:

```Ruby
# Använd puts för att skriva ut en sträng
puts "Hej, programmerare!"

# Använda p för att skriva ut en datastruktur med mer information
p [1, 2, 3]

# Utskrift med variabler
x = "Ruby är kul"
puts x
```

Förväntad utskrift:
```
Hej, programmerare!
[1, 2, 3]
Ruby är kul
```

## Deep Dive:
I Ruby's barndom använde folk `print` för att skriva ut data, men `puts` blev populär då den lägger till en nyrad automatiskt. `p` är ännu mer detaljerad, då den visar en mer "rå" version av objektet, vilket är nyttigt vid debuggning.

Det finns även bibliotek och inbyggda verktyg som `logger` där du kan specificera olika nivåer av felmeddelanden. För stora applikationer är detta att föredra. Biblioteket `awesome_print` ger också färgade och lättlästa utskrifter.

Implementationen av `puts` och `p` är enkel - de anropar objektets `to_s` respektive `inspect` metod och skriver sedan ut resultatet till konsolen.

```Ruby
# Inspect visar objektets interna struktur
puts [1, 2, 3].inspect  # "[1, 2, 3]"
p [1, 2, 3]             # "[1, 2, 3]"

# to_s konverterar objektet till en sträng
puts [1, 2, 3].to_s  # "123"
```

## See Also:
- Ruby Dokumentation för `puts`: https://ruby-doc.org/core-2.7.0/IO.html#method-i-puts
- Ruby Dokumentation för `p`: https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-p
- Ruby `logger` bibliotek: https://ruby-doc.org/stdlib-2.7.0/libdoc/logger/rdoc/Logger.html
- `awesome_print` gem: https://rubygems.org/gems/awesome_print/
