---
title:                "Elixir: Convertendo uma string para letras minúsculas"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que
O Elixir é uma linguagem de programação funcional que está ganhando cada vez mais popularidade entre os desenvolvedores. Uma das tarefas comuns na programação é converter uma string para letras minúsculas. Isso pode ser útil ao manipular dados de entrada ou ao validar informações do usuário. Neste post, vamos explorar como fazer isso em Elixir e mergulhar mais fundo nesse tópico.

## Como fazer
A conversão de uma string para letras minúsculas em Elixir é fácil e direta usando a função `String.downcase/1`. Essa função recebe uma string como parâmetro e retorna uma string com todas as letras em minúsculo. Vamos ver alguns exemplos:

```Elixir
# Exemplo 1
String.downcase("OLÁ MUNDO")
# => "olá mundo"

# Exemplo 2
String.downcase("Elixir é uma linguagem incrível!")
# => "elixir é uma linguagem incrível!"
``` 

Como você pode ver, a função `String.downcase/1` transforma todas as letras maiúsculas em minúsculas, independentemente da posição ou idioma.

## Mergulho profundo
A função `String.downcase/1` funciona usando o módulo `:unicode`, que lida com strings unicode em Elixir. Isso significa que a conversão para minúsculas é feita de acordo com as regras da linguagem do sistema operacional em que o código está sendo executado.

Além disso, a função `String.downcase/1` também lida com casos especiais, como a conversão das letras 'İ' e 'I' na turquia, que não seguem o padrão inglês. Isso garante que sua aplicação seja capaz de lidar com diferentes idiomas e caracteres de forma correta.

## Veja também
- [Documentação da função String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Elixir School - Strings](https://elixirschool.com/pt/lessons/basics/binary-and-strings/)
- [Blog - A modernização da linguagem de programação Elixir](https://blog.saya.academy/a-modernizacao-da-linguagem-de-programacao-elixir/)