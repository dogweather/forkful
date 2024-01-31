---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:48:21.535961-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

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
