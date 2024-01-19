---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & Por quê?

O ato de pesquisar e substituir texto é simplesmente o processo de encontrar determinadas strings ou padrões em um texto e substituí-las por algo diferente. Programadores fazem isso para manipulação de dados, correções em massa ou para implementar funcionalidades em seus aplicativos.

## Como fazer:
Elixir, por ser uma linguagem funcional, fornece funções incorporadas para substituir texto. Aqui está um pequeno exemplo usando a função `String.replace/3` na versão atual do Elixir:

```elixir
texto = "Elixir é incrível."
novo_texto = String.replace(texto, "incrível", "fantástico")

IO.puts(novo_texto)
```

E a saída será:

```elixir
"Elixir é fantástico."
```

## Mergulhando mais fundo
A função `String.replace/3` foi adicionada no Elixir 1.0 e é o mecanismo padrão para pesquisar e substituir texto desde então. Alternativas a isso envolvem o uso de expressões regulares ou a funções definidas pelo usuário, mas na maioria dos casos, `String.replace/3` é o que você irá usar.

Em termos de implementação, Elixir, como Erlang, efetivamente imutável, o que significa que, quando você usa `String.replace/3`, uma nova string é criada com as substituições, sem modificar a original.

## Veja também
Aqui estão alguns links para você explorar mais:

- Documentação oficial do Elixir para `String.replace/3`: [https://hexdocs.pm/elixir/String.html#replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- Uma discussão detalhada sobre expressões regulares em Elixir: [https://elixir-lang.org/getting-started/regex.html](https://elixir-lang.org/getting-started/regex.html)
- Uma explicação das strings no Elixir: [https://howelixirworks.com/post/how_elixir_handles_strings/](https://howelixirworks.com/post/how_elixir_handles_strings/)