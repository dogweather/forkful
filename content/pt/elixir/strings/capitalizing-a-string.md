---
title:                "Capitalizando uma string"
aliases: - /pt/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:04:43.830932-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Capitalizar uma string envolve converter a primeira letra da string para maiúscula enquanto garante que as restantes letras estejam em minúscula. Essa ação é comumente necessária para formatar a entrada do usuário ou exibir texto em interfaces de usuário, onde a consistência e a legibilidade são importantes.

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
