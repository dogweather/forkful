---
title:                "Elixir: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
Att arbeta med JSON (JavaScript Object Notation) är en viktig del av modern programmering. JSON är ett enkelt, läsbart och flexibelt format som används för att överföra data mellan olika applikationer. I denna bloggpost kommer vi att gå igenom hur man kan använda Elixir för att hantera JSON-data effektivt.

## Så här gör du
För att börja arbeta med JSON i Elixir, behöver vi inkludera modulen `Poison` i vårt projekt. Denna modul ger oss funktioner för att konvertera Elixir-termer till JSON och vice versa. Låt oss ta en titt på några exempel:

```elixir
# Konvertera en Elixir-map till JSON
Poison.encode!(%{name: "Anna", age: 25})
# => "{\"name\":\"Anna\",\"age\":25}"

# Konvertera en JSON-sträng till en Elixir-map
Poison.decode!("{\"name\":\"Anna\",\"age\":25}")
# => %{name: "Anna", age: 25}
```

I exemplet ovan använder vi `Poison.encode!/1` för att konvertera en Elixir-map till en JSON-sträng och `Poison.decode!/1` för att konvertera en JSON-sträng till en Elixir-map. Notera att `!` efter funktionerna indikerar att de är destruktiva, vilket innebär att de kommer att kasta ett undantag om konverteringen misslyckas.

## Djupdykning
För att arbeta med JSON-data mer avancerat, kan vi använda `Poison.Encoder` och `Poison.Decoder` modulerna för att anpassa hur vår data konverteras. Till exempel kan vi konvertera en Elixir-tupel till en JSON-array genom att definiera vår egen kodning:

```elixir
defimpl Poison.Encoder, for: Tuple do
  def encode(tuple, options) do
    Poison.Encoder.List.encode(Tuple.to_list(tuple), options)
  end
end
```

När `Poison` stöter på en tuple kommer den nu att konvertera den till en JSON-array istället för en map. Detta ger oss större flexibilitet vid konvertering av data.

## Se även
- [Poison ExDoc](https://hexdocs.pm/poison/api-reference.html)
- [JSON i Elixir](https://elixir-lang.org/getting-started/mix-otp/working-with-external-apis.html#json-in-elixir)
- [Javascript Object Notation (JSON)](https://www.json.org/json-sv.html)