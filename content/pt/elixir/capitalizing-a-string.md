---
title:    "Elixir: Capitalizando uma string"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que Capitalizar uma String em Elixir?

Capitalizar uma string é um processo comum em programação que envolve deixar todas as letras iniciais maiúsculas. Isso pode ser útil para estilizar títulos, nomes próprios ou até mesmo para padronizar dados de entrada em um programa. Felizmente, em Elixir, existem algumas maneiras simples de capitalizar uma string.

## Como Fazer

Para capitalizar uma string em Elixir, podemos usar a função `String.capitalize/1` ou `String.upcase/1`. Vamos dar uma olhada em alguns exemplos abaixo:

```
Elixir> String.capitalize("elixir blog")
"Elixir blog"
```

```
Elixir> String.capitalize("ola Mundo!")
"Ola mundo!"
```

```
Elixir> String.upcase("letras maiusculas")
"LETRAS MAIUSCULAS"
```

```
Elixir> String.upcase("hello 123")
"HELLO 123" 
```

Podemos ver como a função `String.capitalize/1` deixa apenas a primeira letra da string como maiúscula, enquanto `String.upcase/1` deixa todas as letras maiúsculas. Isso pode ser útil dependendo da situação em que estamos usando essas funções.

## Imersão Profunda

Além das funções mencionadas acima, Elixir também possui a função `String.capitalize/2`, que nos permite especificar um idioma para capitalizar a primeira letra da string. Isso pode ser útil quando estamos trabalhando com idiomas que possuem regras específicas de capitalização, como o alemão.

Outra coisa interessante é que, em Elixir, strings são representadas como listas de caracteres, então podemos capitalizar a primeira letra de uma lista usando `List.first/1` e a função `String.upcase/1`. Vamos dar uma olhada em um exemplo:

```
Elixir> char_list = ['p', 'o', 'r', 't', 'u', 'g', 'a', 'l']
Elixir> [first | rest] = char_list
Elixir> [String.upcase(first) | rest]
['P', 'o', 'r', 't', 'u', 'g', 'a', 'l']
```

Isso pode ser útil se quisermos capitalizar a primeira letra de cada palavra em uma string ou lista.

## Veja Também

- [Função `String.capitalize/1` em Elixir](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Função `String.upcase/1` em Elixir](https://hexdocs.pm/elixir/String.html#upcase/1)
- [Função `String.capitalize/2` em Elixir](https://hexdocs.pm/elixir/String.html#capitalize/2)