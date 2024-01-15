---
title:                "Arbeta med json"
html_title:           "Elixir: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON är en vanlig format för att dela och lagra data, vilket gör den viktig för många utvecklare som arbetar med webbtjänster och applikationer. Att lära sig hur man hanterar JSON i Elixir ger dig möjligheten att skapa effektiva och pålitliga system som överför och lagrar data på ett strukturerat sätt.

## Hur man gör

För att hantera JSON i Elixir, behöver du först lägga till biblioteket "Poison" i ditt projekt. Detta bibliotek ger oss funktioner för att konvertera data till och från JSON-format.

```elixir
# Lägger till "Poison" biblioteket
defp deps do
  [
    {:poison, "~> 3.1"}
  ]
end
```

När detta är gjort kan vi använda funktionen `Poison.encode!/2` för att konvertera Elixir-data till JSON-format och `Poison.decode!/2` för att konvertera från JSON till Elixir-data.

```elixir
# Skapar en lista med data
data = ["Elixir", "är", "ett", "språk"]

# Konverterar till JSON och skriver ut resultatet
data_json = Poison.encode!(data)
IO.puts(data_json) # ["Elixir","är","ett","språk"]

# Konverterar tillbaka till Elixir-data och skriver ut resultatet
new_data = Poison.decode!(data_json)
IO.inspect(new_data) # ["Elixir", "är", "ett", "språk"]
```

Om vi har ett objekt istället för en lista, använder vi funktionerna `Poison.encode!/1` och `Poison.decode!/1` för att konvertera till och från JSON. 

```elixir
# Skapar ett enkelt objekt
user = %{name: "Anna", age: 30}

# Konverterar till JSON och skriver ut resultatet
user_json = Poison.encode!(user)
IO.puts(user_json) # {"name":"Anna","age":30}

# Konverterar tillbaka till Elixir-data och skriver ut resultatet
new_user = Poison.decode!(user_json)
IO.inspect(new_user) # %{name: "Anna", age: 30}
```

## Djupdykning

För att hantera mer komplexa strukturer eller för att göra mer avancerade operationer med JSON, kan vi använda funktionerna `Poison.encode/2` och `Poison.decode/2`. Dessa funktioner returnerar en `{:ok, data}` eller `{:error, message}` tuple, vilket gör det möjligt för oss att hantera fel och felhantering.

Att hantera JSON i Elixir ger också möjligheten att arbeta med externa API:er genom att konvertera deras svar till Elixir-data och sedan arbeta med den som vanligt. Det finns också flera andra bibliotek som erbjuder avancerade funktioner för att hantera JSON i Elixir, som exempelvis "Jason" och "JSEX".

## Se även

- [ElixirDocks - Poison](https://elixirdocs.org/poison/readme.html)
- [ElixirSchool - Working with JSON](https://elixirschool.com/en/lessons/advanced/working-with-JSON/)
- [ElixirStyle - Using JSON in Elixir](https://github.com/christopheradams/elixirstyle/blob/master/sections/3-data-structures.md#using-json-in-elixir)