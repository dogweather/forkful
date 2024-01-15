---
title:                "Att göra en sträng versal"
html_title:           "Elixir: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Ibland kan man behöva ändra en sträng så att varje ord börjar med en stor bokstav. Det kan vara för att göra texten mer lättläst eller för att följa vissa formatkrav. I denna artikel kommer vi att titta på hur man kan göra detta i Elixir.

## Så här gör du
För att ändra en sträng till stora bokstäver i Elixir, finns det ett inbyggt funktion som heter `String.capitalize/1`. Vi kan använda den på följande sätt:

```Elixir
String.capitalize("hej världen") 
```

Detta kommer att ge oss resultatet `"Hej världen"`. Vi kan också använda denna funktion på en lista av ord:

```Elixir
"jag gillar att läsa böcker" |> String.split |> Enum.map(&String.capitalize/1) |> Enum.join(" ")
```

Detta ger oss `"Jag Gillar Att Läsa Böcker"` som output.

## Djupdykning
För att förstå hur denna funktion fungerar så behöver vi titta på dess implementation i Elixir. Vi ser då att den faktiskt använder sig av en annan funktion som heter `String.capitalize_first/1` som ändrar den första bokstaven i en sträng till en stor bokstav. Därefter använder den sig av funktionen `String.downcase/1` för att göra resten av strängen till små bokstäver. Det är alltså en kombination av dessa två funktioner som ger oss det önskade resultatet.

## Se även
- [Elixir Dokumentation för `String.capitalize/1`](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Elixir Dokumentation för `String.capitalize_first/1`](https://hexdocs.pm/elixir/String.html#capitalize_first/1)
- [Elixir Dokumentation för `String.downcase/1`](https://hexdocs.pm/elixir/String.html#downcase/1)