---
title:                "Ta bort citattecken från en sträng"
aliases:
- /sv/elixir/removing-quotes-from-a-string.md
date:                  2024-01-26T03:39:25.677192-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ta bort citattecken från en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort citattecken från en sträng innebär att man avlägsnar dessa extra omslag för att få fram den klara texten inuti. Programmerare gör detta för att sanera indata, undvika fel och förbereda data för behandling där citattecken är hinder, inte funktioner.

## Hur man gör:
Elixir har ingen inbyggd funktion för att ta bort citattecken, men det är enkelt att skapa din egen med mönstermatchning eller `String`-funktioner. Se dessa kodsnuttar:

```elixir
# Använda mönstermatchning
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Exempelanvändning
unquote_string("\"Hej, Världen!\"") # => "Hej, Världen!"
unquote_string("'Hej, Världen!'")   # => "Hej, Världen!"

# Använda String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Exempelanvändning
unquote_string("\"Hej, Världen!\"") # => "Hej, Världen!"
unquote_string("'Hej, Världen!'")   # => "Hej, Världen!"
```

Utskriften för båda metoderna blir:
```
"Hej, Världen!"
```

## Fördjupning
Förr i tiden var citattecken i strängar ett minfält – hantera dem fel, och bam, syntaxfel eller säkerhetshål. I Elixir låter mönstermatchning dig behandla dina strängar som Lego-block, vilket gör att du kan plocka isär och bygga om med precision. Dess robusta `String`-modul är också praktisk för att flexibelt nixa citattecken med `trim`-funktioner. Alternativen? Reguljära uttryck kan sparka bort citattecken, och externa bibliotek kan erbjuda extra eldkraft om du behöver mer än grundläggande avlägsnande.

## Se även
Fördjupa dig ytterligare med dessa:
- [Elixirs String-modul](https://hexdocs.pm/elixir/String.html)
- [Lär dig mer om mönstermatchning i Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Reguljära uttryck i Elixir (Regex-modulen)](https://hexdocs.pm/elixir/Regex.html)
