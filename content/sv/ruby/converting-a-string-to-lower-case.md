---
title:    "Ruby: Konvertera en sträng till gemener"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför 

Att konvertera en sträng till gemener (lower case) är en viktig del av Ruby-programmering då det gör det lättare att hantera och manipulera textsträngar. Detta är speciellt användbart vid hantering av användarinput eller sökningar i en databas då den sökta strängen ofta måste matcha den lagrade strängen i gemener.

## Hur man gör det

För att konvertera en sträng till gemener i Ruby, använd metoden `downcase`. Exempelvis, om vi vill konvertera strängen "HEJ" till gemener:

```
HEJ.downcase
```

Detta kommer returnera strängen "hej". Om vi applicerar metoden på en befintlig variabel:

```
text = "HELLO"
text.downcase
```

Vilket kommer att resultera i att variabeln "text" blir tilldelad värdet "hello".

## Djupdykning

När vi använde `downcase` metoden förväntade vi oss att strängen endast skulle innehålla gemener efter konverteringen. Men vad händer om vår ursprungliga sträng redan innehöll gemener? Låt oss ta en titt på följande exempel:

```
"Hello".downcase
```

Output kommer fortfarande att vara "hello". Detta beror på att `downcase` endast konverterar stora bokstäver till små, utan att ändra eventuella redan befintliga gemener.

Det är även viktigt att notera att `downcase` inte bara fungerar för engelska bokstäver, utan även för alla andra bokstavstecken i det Unicode-teckenuppsättning som Ruby stödjer.

## Se även

* [Ruby Dokumentation - downcase](https://ruby-doc.org/core-2.5.1/String.html#method-i-downcase)
* [Ruby Tutorial - Strings](https://www.rubyguides.com/2015/06/ruby-strings/)
* [Ruby Cookbook - Manipulating Strings](https://www.oreilly.com/library/view/ruby-cookbook/0596523696/ch04s08.html)