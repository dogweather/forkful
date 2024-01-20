---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regex, eller regulære uttrykk, lar deg søke etter mønstre i tekst. Programmerere bruker det for effektivt å finne, erstatte eller validere tekstdata.

## Hvordan:
Bruk `Regex` modulen i Elixir:

```elixir
text = "Elixir er magi for programmerere."
pattern = ~r/programmerere/

# Finne et match
match = Regex.run(pattern, text)
IO.inspect(match) # => ["programmerere"]

# Finne alle matcher
matches = Regex.scan(pattern, text)
IO.inspect(matches) # => [["programmerere"]]

# Erstatte tekst
replaced_text = Regex.replace(pattern, text, "utviklere")
IO.puts(replaced_text) # => "Elixir er magi for utviklere."

# Sjekk om et mønster finnes i teksten
if Regex.match?(pattern, text) do
  IO.puts "Fant et match!"
else
  IO.puts "Ingen treff."
end
```

## Deep Dive
Regular expressions har eksistert siden 1950-tallet og er integrert i mange programmeringsspråk. Alternativer inkluderer string-funksjoner, men de mangler fleksibiliteten til regex. Elixir bruker `Regex` modulen, som er basert på Erlangs `:re` modul og bygger på Perl Compatible Regular Expressions (PCRE)-biblioteket.

## See Also
- [Elixir Regex Docs](https://hexdocs.pm/elixir/Regex.html)
- [Erlang :re module](https://erlang.org/doc/man/re.html)
- [Regular-Expressions.info](https://www.regular-expressions.info) for en omfattende guide.
- [Rubular](http://rubular.com/) for å teste Regex mønstre i Ruby (nær Elixir).