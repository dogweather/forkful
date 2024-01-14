---
title:    "Elixir: Radera tecken som matchar ett mönster"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att radera tecken som matchar ett mönster är en vanlig uppgift inom programmering och görs ofta för att rensa data eller för att transformera en sträng på ett specifikt sätt. I Elixir finns det flera olika metoder för att åstadkomma detta och i denna bloggpost kommer vi att utforska de olika alternativen.

## Hur man gör

För att radera tecken som matchar ett mönster i Elixir finns det flera inbyggda funktioner och metoder att använda. Här är några av de vanligaste:

```Elixir
# String.replace/4 tar emot en sträng, mönstret som ska matchas, det nya värdet och en optionell parameter för antal ersättningar som ska göras.

String.replace("Hello World", "o", "a")
# Output: "Hella Warld"

# String.upcase/1 konverterar alla tecken i en sträng till versaler.
String.upcase("john")
# Output: "JOHN"

# Regex.replace/3 fungerar på samma sätt som String.replace/4 men tar emot ett reguljärt uttryck istället för ett enkelt mönster.
Regex.replace("123ABC456DEF", ~r/\d+/, "X")
# Output: "XABCXDEF"
```

Det finns också flera alternativ för att radera tecken från en lista eller en tupel, beroende på ditt specifika behov.

## Gå djupare

Om du vill ha mer kontroll över hur tecken raderas kan du använda dig av reguljära uttryck i kombination med Elixir's Pattern Matching. Till exempel kan du matcha och ta bort en viss del av en sträng istället för hela mönstret:

```Elixir
str = "Hello John Doe"
~r/Hello (\w+) Doe/
String.replace(str, ~r/Hello (\w+) Doe/, "Hi \\1")
# Output: "Hi John"

# Här användes \\1 för att extrahera och använda den första matchen (i detta fall "John") i det nya värdet.
```

Du kan också använda dig av regex grupper för att radera delar av en sträng baserat på ett mönster. Till exempel:

```Elixir
str = "abc123def456ghi789"
~r/(\d+)/
Regex.replace(str, ~r/(\d+)/, "")
# Output: "abcdefghi"
```

Genom att förstå hur reguljära uttryck fungerar och hur man använder Pattern Matching på ett effektivt sätt kan du anpassa dina raderingsfunktioner på ett mer avancerat sätt.

## Se även

- [Elixir Docs: String Module](https://hexdocs.pm/elixir/String.html)
- [Elixir Docs: Regex Module](https://hexdocs.pm/elixir/Regex.html)
- [Elixir School: Pattern Matching](https://elixirschool.com/en/lessons/basics/pattern-matching/)