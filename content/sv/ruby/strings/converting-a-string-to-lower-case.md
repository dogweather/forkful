---
title:                "Konvertera en sträng till gemener"
aliases:
- /sv/ruby/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:11.311904-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att alla stora bokstäver i texten förvandlas till små. Programmerare gör detta för att standardisera textdata, till exempel vid sökningar eller jämförelser, där det inte ska spela någon roll om ord är skrivna med stora eller små bokstäver.

## Hur gör man:
Ruby gör det enkelt att konvertera strängar till gemener med metoden `.downcase`. Här är ett snabbt exempel:

```Ruby
original_string = "Hej Världen!"
lowercase_string = original_string.downcase
puts lowercase_string  # Output: "hej världen!"
```

Använd `.downcase!` för att ändra den ursprungliga strängen direkt:

```Ruby
greeting = "Hej Världen!"
greeting.downcase!
puts greeting  # Output: "hej världen!"
```

## Djupdykning
I äldre programmeringsspråk, innan metoderna för strängmanipulation standardiserades, kunde det vara ganska krångligt att omvandla strängar till gemener. Man var tvungen att omvandla varje tecken enskilt, ofta genom att jämföra och manipulera ASCII-värden.

I Ruby finns det, utöver `.downcase`, andra metodvariationer. Metoden `.downcase` hanterar standardlatinbaserad text väl, men Ruby 2.4 och senare versioner erbjuder metoden `.downcase(options)` som ger ökad kontroll över hur unicodetecken hanteras.

När `.downcase` anropas, ser Ruby till att varje bokstav i strängen transformeras till sitt lowercase-ekvivalent om ett sådant finns. Detta görs dynamiskt och med stöd för flera språk, även om vissa specifika tecken utanför ASCII-uppsättningen kan behöva specialhantering.

## Se även
- Ruby-dokumentation om `.downcase`: [Ruby Docs downcase](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- Ruby Style Guide med rekommenderade kodningspraxis: [Ruby Style Guide](https://rubystyle.guide/)
- Stack Overflow diskussioner om konvertering av strängar: [Stack Overflow Ruby](https://stackoverflow.com/questions/tagged/ruby?sort=votes&pageSize=15)
