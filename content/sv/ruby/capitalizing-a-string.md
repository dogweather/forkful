---
title:                "Versalisera en sträng"
html_title:           "Ruby: Versalisera en sträng"
simple_title:         "Versalisera en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När man "kaptaliserar" en sträng i programmeringsspråket Ruby innebär det att man gör om alla små bokstäver till stora bokstäver. Detta kan göras av olika anledningar, till exempel för att göra texten mer lättläst eller för att matcha specifika krav eller format.

## Hur gör man?
För att kaptaliserar en sträng i Ruby kan man använda metoden "capitalize" på en sträng-variabel. Här är ett exempel på kod:

```Ruby
name = "emma"
puts name.capitalize
```

Detta kommer att returnera strängen "Emma" som output. Man kan också använda metoden "upcase" om man vill konvertera hela strängen till versaler, oavsett om det var små eller stora bokstäver från början.

```Ruby
name = "emmA"
puts name.upcase
```

Detta kommer att returnera "EMMA" som output. 

## Djupdykning
Historiskt sett har konventionen varit att namngivning av variabler och metoder i Ruby följer ett "lower snake case" format, vilket innebär att man använder små bokstäver och separerar ord med understreck istället för mellanslag. Dock finns det ingen absolut regel för detta och det är upp till varje programmerare att avgöra vilken konvention som passar bäst för deras projekt.

Alternativen för kapitalisering i Ruby inkluderar också metoden "capitalize!" som gör en permanent ändring på strängen istället för att bara returnera en ny kaptaliserad version. Det finns också möjligheten att använda regexp (regular expressions) för att göra mer avancerade sökningar och ersättningar av delar av en sträng.

## Se även
- [Officiell dokumentation för Ruby String-klassen](https://ruby-doc.org/core-2.7.2/String.html)
- [En lättförståelig guide till regexp i Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)