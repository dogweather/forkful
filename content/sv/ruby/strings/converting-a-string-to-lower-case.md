---
date: 2024-01-20 17:39:11.311904-07:00
description: "Hur g\xF6r man: Ruby g\xF6r det enkelt att konvertera str\xE4ngar till\
  \ gemener med metoden `.downcase`. H\xE4r \xE4r ett snabbt exempel."
lastmod: '2024-03-13T22:44:38.415607-06:00'
model: gpt-4-1106-preview
summary: "Ruby g\xF6r det enkelt att konvertera str\xE4ngar till gemener med metoden\
  \ `.downcase`."
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

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
