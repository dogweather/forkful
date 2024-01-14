---
title:    "Elixir: Att hitta längden på en sträng"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Det finns många olika användningsområden för att hitta längden på en sträng. Det kan vara för att räkna antalet tecken i en text eller för att kontrollera om en sträng är tillräckligt lång för ett visst syfte. Oavsett vad anledningen är så är det en viktig färdighet för alla som arbetar med Elixir-programmering.

## Så här gör du
För att hitta längden på en sträng i Elixir kan du använda funktionen `String.length()` som tar in en sträng som argument. Det här är ett exempel på kod och tillhörande utdata:

```Elixir
str = "Hej världen"
length = String.length(str)

IO.puts "Längden på strängen är #{length}" 

# => Längden på strängen är 11
```

Här använder vi variabeln `str` för att lagra vår sträng och sedan använder vi `String.length()`-funktionen för att hitta längden på strängen. Den returnerade längden lagras sedan i variabeln `length` som sedan skrivs ut med hjälp av `IO.puts()`-funktionen.

## Djupdykning
Det kan vara intressant att veta hur `String.length()`-funktionen fungerar bakom kulisserna. När en sträng skapas lagras den i minnet som en lista av tecken. Dessa tecken har sina egna unika kodpunkter och `String.length()`-funktionen räknar helt enkelt antalet tecken i listan och returnerar det som längden på strängen.

## Se även
- [Elixir dokumentation för `String.length/1`](https://hexdocs.pm/elixir/String.html#length/1)
- [Elixir string primers](https://elixirschool.com/en/lessons/basics/basics/#string-primers)
- [Elixir förklaring av strängkodning](https://elixirschool.com/en/lessons/specifics/strings/#codepoints-and-graphemes)