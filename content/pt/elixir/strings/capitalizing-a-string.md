---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:43.830932-07:00
description: "Como fazer: Elixir oferece uma maneira direta de capitalizar strings\
  \ usando suas fun\xE7\xF5es integradas, sem a necessidade de bibliotecas de terceiros.\
  \ Aqui\u2026"
lastmod: '2024-03-13T22:44:46.221817-06:00'
model: gpt-4-0125-preview
summary: "Elixir oferece uma maneira direta de capitalizar strings usando suas fun\xE7\
  \xF5es integradas, sem a necessidade de bibliotecas de terceiros."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
Elixir oferece uma maneira direta de capitalizar strings usando suas funções integradas, sem a necessidade de bibliotecas de terceiros. Aqui está um exemplo simples:

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

Saída:

```
Elixir programming
```

Para casos em que mais controle ou uma lógica de capitalização mais complexa é necessária, você pode combinar diferentes funções de String. Por exemplo, se você quiser capitalizar cada palavra em uma frase, você pode dividir a frase em palavras, capitalizar cada uma e depois juntá-las novamente:

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

Saída:

```
Elixir Is Fun
```

Embora a biblioteca padrão do Elixir cubra a maioria das necessidades, para manipulação de texto mais matizada, incluindo capitalização avançada de strings, você pode explorar bibliotecas de terceiros, como Cldr para internacionalização, que podem oferecer comportamentos de capitalização específicos de localidade.
