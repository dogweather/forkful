---
title:                "Utvinna delsträngar"
html_title:           "Elixir: Utvinna delsträngar"
simple_title:         "Utvinna delsträngar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig operation som används för att manipulera text på ett effektivt sätt i Elixir. Det kan hjälpa dig att göra särskilda ändringar i befintliga texter eller att skapa nya textsträngar baserade på viss information.

## Hur man gör

```Elixir
# Extrahera en del av texten från en given plats
sträng = "Det här är en textsträng"
delar = String.slice(string, 4..8) # resultatet blir "här"

# Extrahera en del av texten baserat på ett villkor
delar = to_string(42)
String.slice(delar, 0, &1 == "4") # resultatet blir "4"
```

Elixir har en rad inbyggda funktioner som kan hjälpa dig med att extrahera substrängar, inklusive `String.slice` och `String.split`. Dessa funktioner kan användas tillsammans med reguljära uttryck för mer komplexa extractioner.

## Djupdykning

Att extrahera substrängar kan vara användbart när du arbetar med datahantering eller när du vill manipulera text på ett effektivt sätt. Det är också en viktig kunskap att ha när du arbetar med webbutveckling, där du ofta behöver extrahera specifika delar av en textsträng från ett HTTP-svar.

En annan aspekt att tänka på är att Elixir är byggt på Erlang-plattformen, som har stöd för mönstermatchning. Det betyder att du kan utnyttja denna funktion för att effektivt extrahera substrängar från en given text. Det finns också externa bibliotek som kan hjälpa dig med mer avancerade extraktioner, såsom `regexp` eller `re2`.

## Se även

- Officiell Elixir Documentation: https://hexdocs.pm/elixir/String.html 
- Mönstermatchning i Elixir: https://elixir-lang.org/getting-started/pattern-matching.html 
- Externa bibliotek för reguljära uttryck: https://hex.pm/packages/regexp, https://hex.pm/packages/re2