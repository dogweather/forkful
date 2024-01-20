---
title:                "Escrevendo testes"
html_title:           "Gleam: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## O que é e porquê?

Escrever testes é uma parte importante do processo de desenvolvimento de software, onde os programadores criam pequenos programas que verificam se o seu código principal está funcionando corretamente. Os testes ajudam a garantir que o código é robusto e funciona como o esperado.

## Como:

Para escrever testes usando Gleam, importe o módulo `gleam/testing` e use a função `test` passando uma string descritiva e uma expressão a ser verificada. O resultado será `Ok` se a expressão retornar verdadeira, e `Err` se retornar falso.

```Gleam
import gleam/testing

test "O número 2 é igual a 2" {
  expect 2 == 2
}
```

Para executar seus testes, use o comando `gleam test` no terminal e veja os resultados.

## Detalhando mais:

Escrever testes é uma prática comum em desenvolvimento de software, pois ajuda a garantir que mudanças no código não introduzem novos bugs. Existem outras ferramentas para escrever testes em Gleam, como `gleam-expect` e `gleam-assert`, que oferecem diferentes maneiras de verificar suas expressões.

O módulo de testes do Gleam é escrito em Gleam e usa o assertor do `gleam_assert` por baixo dos panos.

## Veja também:
