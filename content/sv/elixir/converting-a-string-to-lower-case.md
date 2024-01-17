---
title:                "Omvandla en sträng till gemener"
html_title:           "Elixir: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till små bokstäver innebär att man ändrar alla bokstäver i en sträng till deras små bokstavsform. Detta är en viktig funktion inom programmering eftersom det gör det möjligt att jämföra strängar på ett mer exakt sätt, oavsett om de är skrivna med stora eller små bokstäver.

## Hur man gör:

```Elixir
str = "HELLO WORLD"
str |> String.downcase() # => "hello world"
```

Enkelt uttryckt använder vi funktionen `String.downcase()` för att omvandla en sträng till sin små bokstavsform. Genom att pipa strängen in i funktionen, returneras ett nytt värde med de ändrade bokstäverna.

## Djupdykning:

Det finns flera olika sätt att utföra en strängomvandling i Elixir, men `String.downcase()` är den mest använda metoden. I tidigare versioner av språket var `String.downcase()` inte inkluderad i standardbiblioteket och man var tvungen att importera den från en extern modul.

En alternativ metod för att konvertera en sträng till små bokstäver är att använda sig av `String.to_lower()` funktionen. Den största skillnaden mellan de två är att `String.downcase()` endast omvandlar stora bokstäver till små bokstäver, medan `String.to_lower()` även omvandlar andra tecken som `åäö` till sina motsvarande små bokstäver.

## Se även:

https://hexdocs.pm/elixir/String.html#downcase/1 for more info on using `String.downcase()`

https://hexdocs.pm/elixir/String.html#to_lower/1 for more info on using `String.to_lower()`

https://elixir-lang.org/getting-started/basic-types.html#strings for general info on strings in Elixir