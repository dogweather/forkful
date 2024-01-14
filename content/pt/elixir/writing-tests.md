---
title:                "Elixir: Escrevendo testes"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Elixir?

Escrever testes é uma prática essencial em qualquer linguagem de programação, mas especialmente importante em Elixir. Os testes garantem que nosso código esteja funcionando corretamente e nos dão a confiança para realizar alterações sem medo de quebrar o sistema. Além disso, escrever testes nos ajuda a entender melhor o funcionamento do código e facilita a manutenção do mesmo.

## Como escrever testes em Elixir

Para escrever testes em Elixir, usamos o módulo `ExUnit`, que já vem incluído na linguagem. Primeiramente, devemos criar um arquivo com o sufixo `_test.exs` para indicar que se trata de um arquivo de testes. Dentro deste arquivo, criamos uma função com o nome `test` e passamos como parâmetro uma descrição do que estamos testando e uma função contendo o código que deve ser executado.

```Elixir
test "soma de dois números" do
  assert 2 + 3 == 5
end
```
No exemplo acima, estamos testando se a soma de dois números resulta em 5. O `assert` verifica se a expressão é verdadeira e, caso contrário, o teste falhará.

Podemos utilizar também a função `refute` para verificar se uma expressão é falsa. Além disso, é possível usar `assert_raise` e `refute_raise` para testar se uma determinada exceção é lançada.

## Aprofundando em testes em Elixir

Em Elixir, é possível utilizar blocos `do..end` ou `do:..` para agrupar testes e nomeá-los com o auxílio da palavra-chave `describe`. Isso ajuda a organizar melhor o código e a entender o que cada teste está cobrindo.

Outra funcionalidade interessante do `ExUnit` é a possibilidade de utilizar testes assíncronos, que são executados em paralelo, utilizando o módulo `ExUnit.Case` e a função `async_test`.

Além disso, é possível utilizar `setup` e `teardown` para executar código antes e depois de cada teste, permitindo que o ambiente seja preparado adequadamente para cada caso de teste.

## Veja também

- [Documentação oficial do `ExUnit`](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School: Testes](https://elixirschool.com/pt/lessons/advanced/testing/)
- [Desenvolvendo com testes em Elixir](https://medium.com/@diegodsgarcia/desenvolvendo-com-testes-em-elixir-d113af6af269)