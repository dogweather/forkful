---
title:                "Söka och ersätta text"
html_title:           "Elixir: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Vi använder text hela tiden, både när vi läser och skriver kod. Ibland behöver vi göra ändringar i våra texter, och det är här söka- och ersätta-funktionen kommer in i bilden. Det kan spara tid och göra vårt arbete mer effektivt, vilket är varför det är en viktig del av programmering.

## Så här gör du

För att söka och ersätta text i Elixir, använder vi funktionerna `String.replace` och `Regex.replace`. Först behöver vi en sträng som vi vill göra ändringar i, och sedan en söksträng och en ersättningssträng. Här är ett exempel på hur vi kan använda `String.replace`:

```Elixir
iex> text = "Välkommen till Elixir!"
"Välkommen till Elixir!"

iex> String.replace(text, "Elixir", "Elixir 1.12")
"Välkommen till Elixir 1.12!"
```

För att söka och ersätta med hjälp av reguljära uttryck, använder vi `Regex.replace`. Här är ett exempel där vi ersätter alla siffror i en sträng med "X":

```Elixir
iex> text = "123 Elixir 456"
"123 Elixir 456"

iex> Regex.replace(~r/\d+/, text, "X")
"X Elixir X"
```

## Djupdykning

Söka- och ersätta-funktionen är väldigt användbar, men det finns några användbara knep som kan göra det ännu mer kraftfullt.

För att ignorera skillnader mellan små och stora bokstäver, kan vi använda `String.replace/4` och `Regex.replace/4` tillsammans med flaggan `:caseless`. Till exempel:

```Elixir
iex> text = "elixir is awesome!"
"elixir is awesome!"

iex> String.replace(text, "elixir", "Elixir", [:caseless])
"Elixir is awesome!"
```

Vi kan också använda `String.replace/4` för att bara ersätta ett visst antal förekomster i en sträng. Detta kan vara användbart om vi bara vill göra en del av ändringarna som vi söker efter. Till exempel:

```Elixir
iex> text = "Elixir is awesome and Elixir is fun!"
"Elixir is awesome and Elixir is fun!"

iex> String.replace(text, "Elixir", "Ruby", global: false)
"Ruby is awesome and Elixir is fun!"
```

## Se även

- [Elixir Dokumentation om String- och Regex-funktioner](https://elixir-lang.org/getting-started/string-patterns.html)
- [Regex Cheatsheet](https://www.rexegg.com/regex-quickstart.html)