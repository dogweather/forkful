---
title:    "Ruby: Kapitalisering av en sträng"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför

Att göra en sträng stor bokstav (capitalizing) kan vara användbart när man vill ändra utseendet på en sträng eller när man behöver följa konventioner i kodning.

## Så här

Det finns flera sätt att göra en sträng stor bokstav i Ruby.

```Ruby
puts "hej".capitalize
puts "hej".upcase
puts "Hej".swapcase
```

Output:

```
Hej
HEJ
hEJ
```

Det första sättet, `capitalize`, gör bara första bokstaven i en sträng stor, medan `upcase` gör alla bokstäver stora och `swapcase` byter bara ut de små och stora bokstäverna i en sträng.

## Djupdykning

När vi tittar närmare på metoden `upcase` så ser vi att den faktiskt använder sig av en konverteringsenhet som heter Unicode. Detta innebär att även andra språks bokstäver kan omvandlas till stora bokstäver, inte bara de traditionella engelska bokstäverna.

Unicode är en standard för att representera tecken från alla olika skriftsystem i världen. Det gör Ruby mycket användbart för internationell utveckling och textmanipulering.

## Se även

- [Ruby dokumentation om string](https://ruby-doc.org/core-2.6/String.html)
- [Guiden till Unicode i Ruby](http://ruby-doc.com/docs/ProgrammingRuby/html/tut_strings.html#S2)
- [Unicode standarden](https://unicode.org/standard/standard.html)