---
title:    "Elixir: Å bruke regulære uttrykk"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor bruke regulære uttrykk (regular expressions)?

Regulære uttrykk er et kraftig verktøy som kan hjelpe deg med å håndtere tekst på en effektiv måte. De er veldig nyttige når det kommer til å søke, erstatte og manipulere tekst i programmering. Ved å forstå regulære uttrykk, kan du gjøre komplekse oppgaver med tekstbehandling mye lettere og mer effektivt.

# Slik bruker du regulære uttrykk i Elixir

I Elixir, brukes regulære uttrykk ved hjelp av `Regex` modulen. La oss se på noen eksempler på hvordan vi kan bruke regulære uttrykk i Elixir:

```Elixir
Regex.match?(~r/hello/, "Hello, world!") # => true
Regex.match?(~r/[A-Z]+/, "hello") # => false
Regex.replace(~r/world/, "Hello, world!", "Universe") # => "Hello, Universe!"
```

I disse eksemplene brukte vi `Regex.match?` for å sjekke om en tekst samsvarer med et spesifikt mønster, og `Regex.replace` for å bytte ut deler av en tekst med en annen string.

# Dykke dypere inn i bruk av regulære uttrykk

Når du blir mer komfortabel med å bruke regulære uttrykk i Elixir, kan du oppdage at det er mange forskjellige muligheter og teknikker for å håndtere tekst med dem. For eksempel kan du bruke fangegrupper for å hente ut spesifikke deler av en tekst som samsvarer med et mønster:

```Elixir
"%{name} is %{age} years old" =~ ~r/(?<name>.+) is (?<age>\d+) years old/
# => true

Regex.named_captures(~r/(?<name>.+) is (?<age>\d+) years old/, "John is 30 years old")
# => [{"name", "John"}, {"age", "30"}]
```

Du kan også bruke regulære uttrykk for å filtrere lister og strømmer ved hjelp av `Enum.filter`:

```Elixir
words = ["hello", "world", "foo", "bar", "baz"]
Enum.filter(words, &Regex.match?(~r/[aeiou]/, &1)) # => ["hello", "world"]
```

Det finnes også mange forskjellige spesialtegn og modifikatorer som kan gjøre dine regulære uttrykk mer presise og fleksible. Det er verdt å undersøke mer om disse for å utnytte fullt ut potensialet til regulære uttrykk.

# Se også

- [Elixir Regex-modulen dokumentasjon](https://hexdocs.pm/elixir/Regex.html)
- [En interaktiv tutorial om regulære uttrykk i Elixir](https://www.rexegg.com/regex-elixir.html)
- [En liste over nyttige regulære uttrykk eksempler for Elixir](https://gist.github.com/cillianderoiste/97ea2d610e0be267f4c2e64c86e0461d)