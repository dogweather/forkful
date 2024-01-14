---
title:                "Fish Shell: Att Göra En Sträng Storbokstäver"
simple_title:         "Att Göra En Sträng Storbokstäver"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Varför
Att kapitalisera en sträng kan vara användbart för att göra visst textinnehåll mer läsbart och för att följa vissa kodningskonventioner.

## Så här gör du
För att kapitalisera en sträng i Fish Shell, använd kommandot `string capitalize`. Här är ett exempel på hur du skulle kunna använda det:

```
Fish Shell
$ string capitalize "hej alla"
Hej Alla
```

Som du kan se så förvandlar `string capitalize` alla första bokstäver till stora bokstäver.

## Djupdykning
När man kapitaliserar en sträng i Fish Shell, måste man vara medveten om att både det första tecknet och alla andra tecken i strängen kommer att kapitaliseras. Detta är viktigt att komma ihåg om man vill behålla en kodningskonvention som bara kapitaliserar det första tecknet i en sträng.

## Se även
- [Fish Shell dokumentation om string](http://fishshell.com/docs/current/cmds/string.html)
- [Översikt över Fish Shell syntax](https://fishshell.com/docs/current/tutorial.html#syntax)
- [En introduktion till Fish Shell för nybörjare](https://medium.com/@sindreij/an-introduction-to-fish-shell-for-beginners-cef7e5b537fe)