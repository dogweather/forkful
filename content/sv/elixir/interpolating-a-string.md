---
title:                "Interpolering av en sträng"
html_title:           "Elixir: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Tristringinterpolering i Elixir är en metod för att kombinera värden från olika variabler eller uttryck i en sträng. Det kan användas för att enkelt skapa dynamiska meddelanden eller bygga komplexa strängar utan att behöva använda flera konkateneringar. Programmerare använder detta för att uppnå mer effektiva och läsbara kodbaser.

## Hur man:
```Elixir
name = "Maria"
age = 25
IO.puts "Hej, mitt namn är #{name} och jag är #{age} år gammal."

```
`Hej, mitt namn är Maria och jag är 25 år gammal.`

I detta exempel kombineras variablerna `name` och `age` med hjälp av `#{}` syntaxen inuti en sträng för att skapa en personlig hälsning. Det är viktigt att strängen är omgiven av dubbla citationstecken för att interpoleringen ska fungera.

## Djupdykning:
Trådinterpolering har funnits sedan de första versionerna av Elixir och har sitt ursprung från språket Ruby. Det är ett populärt sätt att skapa dynamiska strängar i många programmeringsspråk, inklusive JavaScript, Python och Java. Alternativet till trådinterpolering är konkatenering, där variabler och strängar läggs till i strängen med hjälp av `+` tecknet.

Elixir använder sig av en formateringsfunktion, `format_s!`, för att utföra stränginterpolering. Detta ger också programmerare möjlighet att formatera värdena som ska kombineras. Till exempel kan vi använda `%s` för att formatera en sträng och `%d` för att formatera en heltalsvariabel.

## Se även:
- [String Module Documentation](https://hexdocs.pm/elixir/String.html#module-interpolation)
- [Elixir Forum - String Interpolation Thread](https://elixirforum.com/t/string-interpolation-syntax-question/926)
- [The Power of Interpolation in Elixir](https://hashrocket.com/blog/posts/the-power-of-interpolation-in-elixir)