---
title:                "Sökning och ersättning av text"
html_title:           "Gleam: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att söka och ersätta text är en vanlig uppgift för programmerare. Det innebär att man letar efter specifika ord eller fraser i en text och ersätter dem med något annat. Detta är användbart för att snabbt och enkelt göra stora ändringar i koden eller texten.

# Hur man gör:
För att söka och ersätta text i Gleam, använd dig av funktionen `replace` eller `replace_all` beroende på dina behov. Här är ett exempel på hur man skulle kunna söka efter "hello" och ersätta det med "hey" i en sträng:

```Gleam
let str = "hello world"
let new_str = replace_all("hello", "hey", str)
```

Detta kommer resultera i ett nytt värde i `new_str` - "hey world". Om du bara vill ersätta det första förekommandet av "hello", använd funktionen `replace` istället.

# Djupdykning:
Att söka och ersätta text är en viktig del av textmanipulering inom programmering och har funnits med i många språk sedan tidigt 1900-tal. Andra sätt att utföra denna uppgift inkluderar att använda reguljära uttryck eller att använda en specialiserad sök- och ersättningfunktion i ditt textredigeringsprogram.

I Gleam är det möjligt att använda reguljära uttryck genom att importera biblioteket `regex` och använda funktionen `regex_replace`. Detta ger dig ännu mer flexibilitet när du söker och ersätter text.

# Se även:
- [Gleam dokumentation för replace](https://gleam.run/core-lib/string.List.html#function:replace)
- [Gleam dokumentation för replace_all](https://gleam.run/core-lib/string.List.html#function:replace_all)
- [Dokumentation för reguljära uttryck](https://www.regular-expressions.info/)