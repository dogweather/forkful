---
title:    "Elixir: Convertendo uma string para minúsculas."
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que?

Você já se encontrou em uma situação onde precisava trabalhar com um texto em Elixir, mas o caso das letras não era compatível com o que você precisava? Converter uma string para caixa baixa pode ser uma solução simples e eficaz para esse problema.

## Como Fazer?

O Elixir possui uma função embutida, chamada `String.downcase/1`, que faz exatamente isso: converte todas as letras de uma string para minúsculas. Veja um exemplo de como utilizá-la:

```Elixir
iex> String.downcase("EXEMPLO")
"exemplo"
```

Além disso, você também pode utilizar o operador de pipe `|>` para deixar o código ainda mais legível:

```Elixir
iex> "UM TEXTO TODO EM LETRAS MAIÚSCULAS" |> String.downcase()
"um texto todo em letras maiúsculas"
```

## Profundando Mais

Por baixo dos panos, a função `String.downcase/1` utiliza o módulo `String.Ops` e a função `to_lower/2`, que por sua vez chama a função `:unicode.characters_to_lower/1` do módulo `:unicode`. Essa função é responsável por realizar a conversão de acordo com as regras especificadas pela Unicode Consortium.

Além disso, é importante lembrar que a conversão para caixa baixa nem sempre é simples. Em alguns idiomas, caracteres especiais possuem letras maiúsculas e minúsculas distintas, o que pode impactar o resultado ao utilizar a função padrão do Elixir. Nesse caso, é possível utilizar funções específicas para diferentes idiomas, como `:unicode.latin1_to_unicode/1` e `:unicode.latin1_to_latin1_l2/1`.

## Veja Também

- [Documentação do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Funções de conversão do módulo :unicode](https://hexdocs.pm/elixir/Unicode.html)
- [Elixir - Uma linguagem dinâmica e funcional para construção de sistemas escaláveis](https://elixir-lang.org/)