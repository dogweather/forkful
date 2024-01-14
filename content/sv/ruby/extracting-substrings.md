---
title:                "Ruby: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Att extrahera substrängar är en användbar funktion i Ruby som gör det möjligt att ta ut en del av en sträng baserat på ett visst index eller mönster. Detta kan vara användbart för att hantera data, söka igenom text eller göra kod bearbetningar.

## Så här gör du
För att extrahera substrängar i Ruby, använder vi metoden `slice` eller dess aliase `[]`. Den tar in två argument - startindex och längden på substrängen. Här är ett exempel:

```Ruby
strang = "Hej! Det här är en textsträng."

# Extrahera de första fyra tecknen
puts strang[0, 4]
# Output: Hej!

# Extrahera tecknen från och med index 6 till slutet av strängen
puts strang[6..-1]
# Output: Det här är en textsträng.
```

Vi kan också använda metoden `slice` med ett regex mönster för att extrahera substrängar baserat på ett visst mönster. Här är ett exempel på det:

```Ruby
num_strang = "123abc456def"

# Extrahera alla tecken som är siffror
puts num_strang[/\d+/]
# Output: 123456
```

## Djupdykning
Förutom att extrahera substrängar baserat på index eller mönster, kan vi också använda metoden `slice` med block för att utföra avancerade manipulationer på en sträng. Detta låter oss göra saker som att byta ut delar av en sträng eller flytta tecken runt.

Här är ett exempel på hur vi kan använda block med `slice` för att byta ut tecken i en sträng med hjälp av `gsub`:

```Ruby
strang = "Hej! Det här är en textsträng."

# Byt ut alla 'e' med '3'
puts strang.gsub(/e/, '3')
# Output: H3j! D3t här är 3n t3xtsträng.
```

Detta är bara en av många möjligheter när det kommer till att extrahera substrängar. Var kreativ och experimentera för att se vad du kan åstadkomma!

## Se även
- [Ruby's official documentation on strings](https://ruby-doc.org/core-2.7.0/String.html)
- [Codecademy's tutorial on string methods in Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-methods/cheatsheet)