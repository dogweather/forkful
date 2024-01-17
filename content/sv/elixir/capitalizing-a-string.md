---
title:                "Stora första bokstaven i en sträng"
html_title:           "Elixir: Stora första bokstaven i en sträng"
simple_title:         "Stora första bokstaven i en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att göra alla bokstäver i strängen stora. Programmers gör det för att göra strängar mer läsbara och för att följa konventionella namngivningsstandarder.

## Hur man gör:
```Elixir
String.to_uppercase("hej, världen")
```
Aktiverar utmatningen: "HEJ, VÄRLDEN"

```Elixir
String.upcase("hej, världen")
```
Aktiverar också utmatningen: "HEJ, VÄRLDEN"

## Djupdykning:
Att kapitalisera en sträng är en vanlig operation i programmering och finns i flera olika språksyntaxer. Det är vanligtvis en del av en strängbehandlingsmodul och tar emot en sträng som argument. Det finns också alternativ för att bara kapitalisera första bokstaven eller för att kapitalisera specifika tecken. Det kan vara användbart när man hanterar användarinput eller jämför strängar.

## Se även:
[w3schools tutorial](https://www.w3schools.com/python/ref_string_upper.asp) 
[Elixir Docs](https://hexdocs.pm/elixir/String.html#upcase/1)