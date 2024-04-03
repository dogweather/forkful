---
date: 2024-01-20 17:48:21.535961-07:00
description: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken den inneh\xE5ller. Programmerare g\xF6r detta f\xF6r att validera indata,\
  \ formatera utdata eller\u2026"
lastmod: '2024-03-13T22:44:38.419686-06:00'
model: gpt-4-1106-preview
summary: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken den inneh\xE5ller."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken den innehåller. Programmerare gör detta för att validera indata, formatera utdata eller utföra operationer som kräver kännedom om strängens storlek.

## Hur gör man:
I Ruby, använd `.length` eller `.size` på en sträng för att få dess längd.

```ruby
str = "Hej världen!"
puts str.length  # Output: 12
puts str.size    # Output: 12
```

Båda metoderna returnerar samma resultat.

```ruby
tom_strang = ""
puts tom_strang.length  # Output: 0
```

Even an empty string can be checked and will return 0.

## Djupdykning
I tidigare programmeringsspråk var hantering av strängar inte lika direkt. Till exempel, i C bestäms strängens längd genom att söka efter en termineringskaraktär (`\0`) – ett manuellt och ibland buggigt förfarande.

I Ruby är `.length` och `.size` alias för varandra; det spelar ingen roll vilket du använder. Det finns också en metod `.bytesize` som returnerar antalet bytes som strängen upptar, vilket kan skilja sig från `.length` om strängen innehåller flerbyte-tecken, som emoji eller vissa internationella tecken.

```ruby
emoji_strang = "🙂"
puts emoji_strang.length   # Output: 1
puts emoji_strang.bytesize # Output: 4
```

I exemplet ovan har emoji-symbolen en längd av 1 tecken men upptar 4 bytes.

## Se även
- Ruby-dokumentation för strängklassen: [String](https://ruby-doc.org/core/String.html)
- Ruby-dokumentation för strängmetoden `.bytesize`: [String#bytesize](https://ruby-doc.org/core-2.5.1/String.html#method-i-bytesize)
