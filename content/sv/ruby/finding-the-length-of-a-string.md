---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng betyder att räkna antalet tecken i den. Programmerare gör detta för att manipulera data, validera input, eller lösa problem (t.ex., att avgöra om ett användarnamn är för långt).

## Hur man gör:

Använd `String#length` eller `String#size` metod i Ruby för att hitta längden på ett sträng. Här är exempel på kod och utdata:

```Ruby
str = "Hej Sverige"
puts str.length  # Ger 11
puts str.size    # Ger också 11
```

Båda metoderna ger exakt samma resultat, vilket är antalet tecken i strängen.

## Fördjupning

Historiskt sett har både `length` och `size` metoder funnits i Ruby sedan version 1.2. De är precis likadana eftersom `size` är ett alias för `length` - det ger utvecklare fler alternativ att använda beroende på vad som låter mest naturligt för dem.

En alternativ metod för att hitta längden på en sträng är att använda `String#bytesize`, men det ger antalet bytes och inte antalet tecken. Detta är viktigt när man hanterar icke-ASCII eller flerbytes tecken. Till exempel:

```Ruby
str = "hej"
puts str.length      # Ger 3
puts str.bytesize    # Ger 3

str = "hej 😀"        # Innehåller en emoji tecken
puts str.length      # Ger 4
puts str.bytesize    # Ger 8
```

Så, i allmänhet, använd `length` eller `size` för stränglängd, och `bytesize` för bytesize.

## Se även:

- Ruby Docs [`String#length`](https://ruby-doc.org/core-2.7.0/String.html#method-i-length), [`String#size`](https://ruby-doc.org/core-2.7.0/String.html#method-i-size) och [`String#bytesize`](https://ruby-doc.org/core-2.7.0/String.html#method-i-bytesize) för mer information och exempel.
- [Learn Ruby](https://learnrubythehardway.org/book/ex6.html) för mer baslärande om strängar.