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

## Por que escrever testes é importante?

Escrever testes é uma prática fundamental para todo programador, pois garante a qualidade do código e previne possíveis erros em sua aplicação. Além disso, a criação de testes automatizados ajuda a economizar tempo e esforço em futuras manutenções.

## Como fazer isso no Gleam?

A linguagem de programação Gleam possui um sistema de testes integrado que facilita a criação de testes unitários e funcionais. Para criar um teste, basta criar um arquivo com a extensão .test.gleam e importar o módulo de testes da biblioteca padrão.

```Gleam
import test

test.test("Meu teste") {
  assert.equal(2 + 2, 4)
}
```

Ao executar o comando `gleam test`, todos os testes no arquivo serão executados, mostrando o resultado e os possíveis erros encontrados.

## Aprofundando no assunto

Além de simples testes de comparação de valores, o Gleam também permite o uso de blocos de código para realizar testes mais complexos. Além disso, é possível criar testes que simulam erros e falhas para garantir que a aplicação se comportará corretamente nesses cenários.

Veja mais sobre testes no Gleam em sua [documentação oficial](https://gleam.run/book/testing.html).

## Veja também

- [Documentação oficial do Gleam](https://gleam.run/documentation/)
- [Tutorial de introdução ao Gleam](https://medium.com/@gleamlang/apresentando-o-gleam-uma-linguagem-de-programa%C3%A7%C3%A3o-funcional-33c6375d6355)