---
title:                "Sammanfogning av strängar"
html_title:           "Elixir: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

Vad är concatenating strings och varför gör programmerare det?

Concatenating strings refererar till att slå samman två eller flera strängar till en enda sträng. Detta kan vara användbart när du vill kombinera olika texter eller variabler för att skapa en längre sträng.

Detta är ett vanligt förekommande koncept inom programmering eftersom det ger oss möjligheten att manipulera text på ett användbart sätt. Genom att dela upp en sträng i mindre delar och sedan sätta ihop dem på ett specifikt sätt kan vi skapa önskad output.

Hur man gör det:
```Elixir
sträng1 = "Hej "
sträng2 = "världen!"

concat_sträng = sträng1 <> sträng2

IO.puts concat_sträng
```
Output: "Hej världen!"

Det finns flera sätt att concatenating strings i Elixir, men det vanligaste är att använda operatören `<>` (kan också skrivas som `<>`). Detta sätter ihop två strängar till en och returnerar ett nytt värde, medan de ursprungliga strängarna förblir oförändrade.

Mer avancerade funktioner som `String.concat/1` och `String.joint/1` finns också tillgängliga i Elixir för att göra concatenating strings ännu enklare.

Djupdykning:
Concatenating strings är en vanlig koncept inom programmering och är inte unikt för Elixir. Det finns flera andra programmeringsspråk som också erbjuder liknande funktioner, till exempel JavaScripts `+` operator och Pythons `+` operator.

När du jobbar med mycket långa strängar och behöver prestanda, kan det vara mer fördelaktigt att använda metoder som `String.joint/1` istället för `<>` operatorn. Detta beror på hur Elixir implementerar referenser och garbage collection.

Se även:
- Elixir dokumentation för `String` module: https://hexdocs.pm/elixir/String.html
- En bra bloggpost om förbättringar i strängmanipulering i Elixir 1.3: https://lostechies.com/iancooper/2016/06/14/improvements-in-string-manipulation-in-elixir-1-3/
- En artikel om strängkonkatenering i andra programmeringsspråk: https://en.wikipedia.org/wiki/String_append