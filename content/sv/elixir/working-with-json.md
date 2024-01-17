---
title:                "Arbeta med JSON"
html_title:           "Elixir: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON (JavaScript Object Notation) i Elixir är ett vanligt förekommande arbete för programmerare. JSON är ett lättviktigt format för datautbyte, vilket innebär att det används för att överföra data mellan en klient och en server. Det är särskilt användbart för webbapplikationer och API:er.

## Hur man gör:

Det är enkelt att hantera JSON i Elixir tack vare språkets inbyggda moduler. Här är ett enkelt exempel på hur man kan konvertera en sträng av JSON-data till en Elixir-typ:

```Elixir
json_string = "{\"name\":\"Lisa\", \"age\": 25}"
elixir_map = Jason.decode!(json_string)
IO.inspect(elixir_map) # Outputs: %{name: "Lisa", age: 25}
```

För att skicka data som JSON från en Elixir-applikation kan man använda sig av efterföljande kod:

```Elixir
elixir_map = %{name: "Anna", age: 30}
json_string = Jason.encode!(elixir_map)
IO.puts(json_string) # Outputs: {"name":"Anna","age":30}
```

## Djupdykning:

JSON utvecklades ursprungligen av Douglas Crockford för att ersätta XML som det vanligaste sättet att strukturera data på webben. En alternativ metod för att arbeta med JSON i Elixir är att använda biblioteket Poison. Det är ett tredjepartsbibliotek som erbjuder ytterligare funktioner för hantering av JSON-data.

När det gäller implementationen av JSON i Elixir så används biblioteket Jason som bygger på BEAM-porten Jiffy, vilket ger bra prestanda för kodningen och avkodningen av JSON-data.

## Se även:

- Elixirs inbyggda moduler för att hantera JSON: https://hexdocs.pm/elixir/1.12.0/Kernel.html#to_json/1
- Jiffy, BEAM-porten för Jason biblioteket: https://github.com/jeremyong/jiffy
- Poison biblioteket: https://hex.pm/packages/poison