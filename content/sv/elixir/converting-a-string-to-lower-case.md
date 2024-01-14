---
title:    "Elixir: Omvandla en sträng till gemener"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är en vanlig uppgift inom programmering. Det är användbart när man till exempel vill jämföra strängar oberoende av deras stor- och småbokstäver eller när man vill formatera en sträng på ett enhetligt sätt. I denna bloggpost kommer vi att diskutera hur man kan göra detta i Elixir.

## Hur man gör det

I Elixir finns en inbyggd funktion som heter `String.downcase/1` som konverterar en sträng till små bokstäver. Vi kan använda den på följande sätt:
```Elixir
iex> String.downcase("HELLO")
"hello"
```

Det är viktigt att notera att denna funktion inte ändrar den ursprungliga strängen utan returnerar en ny sträng med de små bokstäverna. Vi kan också använda `String.downcase/2` om vi vill konvertera en sträng till små bokstäver baserat på en specifik språkkod. Till exempel:
```Elixir
iex> String.downcase("HELLO", "sv")
"hello"
```

Vi kan också använda en RegExp och funktionen `String.replace/3` för att byta ut de stora bokstäverna till små. I följande exempel använder vi en RegExp för att matcha alla stor bokstäver och ersätter dem med deras motsvarande små bokstäver.
```Elixir
iex> String.replace("HELLO", ~r/[A-Z]/, fn x -> String.downcase(x) end)
"hello"
```

Vi kan också använda `Enum.map/2` för att konvertera en lista av strängar till små bokstäver. Till exempel:
```Elixir
iex> list = ["HeLlO", "wOrLd"]
["HeLlO", "wOrLd"]
iex> Enum.map(list, &String.downcase/1)
["hello", "world"]
```

Som du kan se finns det flera olika sätt att konvertera en sträng till små bokstäver i Elixir beroende på ditt specifika behov.

## Djupdykning

När vi använder `String.downcase/1` funktionen i Elixir, använder den faktiskt en inbyggd Erlang funktion som heter `:unicode.lower/1`. Denna funktion konverterar strängen till Unicode och sedan till små bokstäver. Det är viktigt att notera att detta kan leda till olika resultat beroende på din operativsystemsmiljö och vilken språkkod du använder.

Om du vill ha mer kontroll över konverteringen från stora bokstäver till små, så kan du använda `String.codepoints/1` för att få en lista av alla koder som representerar tecknen i din sträng. Du kan sedan använda `Enum.map/2` eller `Enum.each/2` för att iterera över listan och göra dina egna ändringar. Detta kan vara användbart om du vill inkludera specifika tecken eller om du jobbar med andra teckenuppsättningar än Unicode.

## Se också

- [String modulen i Elixir docs](https://hexdocs.pm/elixir/String.html)
- [Unicode modulen i Erlang docs](https://erlang.org/doc/man/unicode.html)
- [Regular expressions i Elixir docs](https://hexdocs.pm/elixir/Regex.html)
- [Enum modulen i Elixir docs](https://hexdocs.pm/elixir/Enum.html)