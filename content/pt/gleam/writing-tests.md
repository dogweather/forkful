---
title:                "Gleam: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-tests.md"
---

{{< edit_this_page >}}

##Por que escrever testes em Gleam

A escrita de testes em qualquer linguagem de programação é crucial para garantir que o código está funcionando corretamente e prevenir falhas no futuro. Em Gleam, a prática de escrever testes é especialmente importante devido à sua tipagem forte, que ajuda a capturar erros de forma mais precisa e evitar problemas de execução em produção.

## Como escrever testes em Gleam

Escrever testes em Gleam é simples e direto. Para começar, é importante importar o módulo de teste no seu arquivo:

```Gleam
import gleam/test
```

Em seguida, defina sua função de teste usando o padrão `test/0`:

```Gleam
pub test/0 = {
  assert.eql(2, 1 + 1)
}
```

Este é um exemplo básico de teste que verifica se 1 + 1 é igual a 2. Você pode usar outras funções de asserção, como `assert.lt/2` para verificar se um valor é menor que outro. Para executar seus testes, você pode usar o comando `gleam test` no terminal.

## Mergulho Profundo

Além das funções de asserção básicas mencionadas acima, Gleam também possui a função `assert.eql/2`. Esta função é útil quando se trabalha com tipos de dados mais complexos, como tuplas e registros. Por exemplo:

```Gleam
type Alias = { id: Int, name: String }
let item = { id: 1, name: "Item 1" }

test "check alias equality" {
  assert.eql(item, { id: 1, name: "Item 1" })
}
```

Este é apenas um exemplo simples, mas é importante lembrar que escrever testes em Gleam permite que você verifique a precisão de seus tipos de dados, o que pode ser crucial em projetos maiores.

## Veja também

- [Documentação Gleam: Testes](https://gleam.run/book/getting-started/test.html)
- [Artigo sobre a importância de testes em tipagem forte](https://blog.feathersapp.com/why-strong-typing-is-your-unit-test/)
- [Exemplo de código com testes em Gleam](https://github.com/gleam-lang/gleam/tree/master/core/src/test)