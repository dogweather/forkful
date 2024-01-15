---
title:                "Hitta längden på en sträng"
html_title:           "Elixir: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng kan vara en viktig del av programutveckling för att kunna manipulera och hantera data på ett effektivt sätt. Det kan också vara en grundläggande färdighet som behövs för att lösa komplexa problem och skapa mer sofistikerade algoritmer.

## Såhär gör du

För att hitta längden på en sträng i Elixir kan vi använda funktionen `String.length()`, som returnerar antalet tecken i en sträng. Vi kan också använda `String.codepoints()` som returnerar en lista av kodpoängen för varje tecken i en sträng.

```Elixir
iex> String.length("Hej!")
4
iex> String.codepoints("Hej!")
[72, 101, 106, 33]
```

För att kontrollera hur många bytes en sträng tar upp i minnet kan vi använda funktionen `byte_size()`, som returnerar antalet bytes som en sträng innehåller.

```Elixir
iex> byte_size("Hej!")
4
```

Vi kan också använda `String.length()` för att hitta längden på en sträng som använder multibyte-tecken, som t.ex. i det japanska alfabetet.

```Elixir
iex> String.length("こんにちは")
5
```

## Djupdykning

Det finns en viktig skillnad mellan `String.length()` och `byte_size()` - den första mäter längden på en sträng i antal tecken, medan den senare mäter längden på en sträng i antal bytes. Detta beror på att vissa tecken kan ta upp mer än en byte i minnet, särskilt om de tillhör ett skriftspråk som använder multibyte-tecken.

Det är också viktigt att vara medveten om att olika encoding-system kan påverka hur många bytes en sträng tar upp i minnet. Om du arbetar med strängar som innehåller specialtecken, är det en god idé att använda utf-8 encoding, som stöds av de flesta moderna verktyg och språk.

## Se även

- [Elixir String Modul](https://hexdocs.pm/elixir/String.html#content)
- [Utf-8 Encoding](https://en.wikipedia.org/wiki/UTF-8)