---
title:                "Omvandling av en sträng till gemener"
html_title:           "Elixir: Omvandling av en sträng till gemener"
simple_title:         "Omvandling av en sträng till gemener"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) är ett vanligt behov för många utvecklare. Det kan vara användbart för sortering, sökning och jämförelse av strängar.

## Hur man gör det

```Elixir
str = "HELLO WORLD"
IO.puts String.downcase(str)
```
Output:
```Elixir
hello world
```

Det enklaste sättet att konvertera en sträng till gemener i Elixir är genom att använda funktionen `String.downcase()`. Denna funktion tar en sträng som argument och returnerar en ny sträng med alla bokstäver i gemener.

Det är också möjligt att använda `String.downcase()` för att konvertera enstaka bokstäver till gemener, genom att först konvertera bokstaven till en sträng:

```Elixir
char = "A"
IO.puts String.downcase(char)
```
Output:
```Elixir
a
```

## Utforska djupare

En viktig sak att notera är att `String.downcase()` endast konverterar bokstäver som finns i det latinska alfabetet till gemener. Andra tecken och bokstäver från andra alfabet, som till exempel grekiska eller kyrilliska, kommer inte att konverteras.

Om du behöver konvertera tecken från andra alfabet eller tecken som inte ingår i standardteckensnittet, kan du använda funktionen `String.downcase/2`. Denna funktion tar ett andra argument som är en lista över alla tecken som ska konverteras till gemener.

Som ett exempel, om du vill konvertera tecken från det grekiska alfabetet till gemener, kan du använda följande kod:

```Elixir
str = "Γεια σου Κόσμε"
IO.puts String.downcase(str, :greek)
```

Output:
```Elixir
γεια σου κόσμε
```

## Se också

- [Officiell dokumentation för `String.downcase()`](https://hexdocs.pm/elixir/String.html#downcase/2)
- [Stack Overflow-fråga om konvertering av tecken från olika alfabet](https://stackoverflow.com/questions/46588736/how-to-convert-character-to-lowercase-if-its-unicode)