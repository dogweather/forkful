---
title:                "Escrevendo testes"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (O Que & Por Que?)
Escrever testes é criar verificações que asseguram que seu código faz o que você espera. Programadores testam para pegar bugs antes do lançamento, economizar tempo e estresse, e para criar um código mais confiável.

## How to (Como Fazer):
```gleam
import gleam/should
import my_module

pub fn my_test() {
  my_module.my_function() 
  |> should.equal("expected result")
}

// Saída esperada da execução do teste
// OK my_module:my_test
```

## Deep Dive (Mergulho Profundo)
Testes unitários em Gleam seguiram o caminho trilhado por linguagens funcionais antecessoras, como Erlang e Elixir. Há alternativas como testes de integração e TDD (Test Driven Development). Em Gleam, os detalhes de implementação dos testes são diretos: use o módulo `should` para expressar expectativas e o próprio compilador lida com o resto.

## See Also (Veja Também)

Lembre-se: testar é cuidar do seu código!
