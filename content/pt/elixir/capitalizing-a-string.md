---
title:                "Elixir: Maiúscula de uma string."
simple_title:         "Maiúscula de uma string."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Elixir?

Capitalizar uma string é uma tarefa comum em muitas linguagens de programação, incluindo Elixir. Ao capitalizar uma string, é possível padronizar o estilo do texto e torná-lo mais legível para os usuários. Além disso, em algumas situações, é necessário que as strings estejam capitalizadas para que as funções de pesquisa e classificação funcionem corretamente.

## Como capitalizar uma string em Elixir

Em Elixir, a função `String.capitalize/1` é responsável por capitalizar uma string. Ela aceita uma string como entrada e retorna uma nova string com a primeira letra em maiúscula. Veja o exemplo abaixo:

```Elixir
nome = "joão"
String.capitalize(nome)
# Output: "João"
```

Se a string já estiver capitalizada, a função irá simplesmente retorná-la sem fazer nenhuma alteração. Além disso, a função `String.capitalize/1` também funciona com caracteres especiais e acentos, garantindo que a string seja capitalizada corretamente independentemente do idioma utilizado.

## Mergulho profundo na capitalização de strings

Embora a função `String.capitalize/1` seja a forma mais comum de capitalizar uma string em Elixir, existem outras opções disponíveis. Por exemplo, a função `String.upcase/1` converte todos os caracteres em maiúsculos e a função `String.capitalize\_words/1` capitaliza cada palavra separadamente.

Além disso, se você precisar capitalizar uma string com um estilo específico, como o estilo "título", existe um módulo `String.Case` que oferece funções específicas para diferentes tipos de capitalização.

## Veja também

- [Documentação oficial da função `String.capitalize/1`](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Tutoriais com outras formas de capitalizar uma string em Elixir](https://elixirforum.com/t/capitalizing-a-string-in-elixir/6662)
- [Explicação detalhada sobre as diferentes opções de capitalização em Elixir](https://elixirnation.io/learning-to-code-in-elixir-why-you-should-begin-with-a-language-style/)