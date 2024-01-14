---
title:    "Elm: Escrevendo testes"
keywords: ["Elm"]
---

{{< edit_this_page >}}

#
## Por que escrever testes em Elm?

Escrever testes é uma prática comum em programação, mas por que fazemos isso exatamente? Bom, testes nos ajudam a garantir que nosso código está funcionando corretamente e que não ocorrerão problemas inesperados no futuro. Além disso, eles nos ajudam a identificar e corrigir bugs antes mesmo de colocar nosso código em produção. Em resumo, escrever testes é uma forma de garantir a qualidade do nosso código e tornar o processo de desenvolvimento mais eficiente.

## Como escrever testes em Elm

Em Elm, podemos escrever testes usando o módulo nativo `Test`. Primeiro, importamos o módulo em nosso arquivo e, em seguida, definimos nossa suíte de testes usando a função `describe` e seus argumentos `description` e `tests`. Dentro dos testes, usamos a função `test` para definir os casos de teste, fornecendo uma descrição e uma função que retorna um valor `Expect`, que é verificado em relação ao valor esperado.

Aqui está um exemplo de como isso pode ser feito:

```Elm
import Test exposing (..)

suite =
  describe "Testes matemáticos" [ -- Definindo uma suíte de teste com descrição e uma lista de testes
    test "2 + 2 deve ser igual a 4" (
      expect (2 + 2) toBe 4   -- Verificando se 2 + 2 é igual a 4
    ),
    test "5 - 3 deve ser igual a 2" (
      expect (5 - 3) toBe 2   -- Verificando se 5 - 3 é igual a 2
    )
  ]

main =
  run suite   -- Rodando nossa suíte de testes
```

E a saída seria:

```Elm
Testes matemáticos
- 2 + 2 deve ser igual a 4 : PASSED
- 5 - 3 deve ser igual a 2 : PASSED

Passed: 2
Failed: 0
```

## Aprofundando mais em testes em Elm

Existem diferentes tipos de testes que podemos escrever em Elm, como testes unitários e testes de integração. Além disso, podemos utilizar ferramentas como `testdouble` para criar mocks e spies para isolar nosso código e facilitar os testes. Em geral, é importante seguir boas práticas ao escrever testes, como manter os testes independentes e focar apenas na funcionalidade sendo testada.

Além disso, uma prática comum é seguir o TDD (Test Driven Development), escrevendo os testes antes mesmo de começar a implementar a funcionalidade. Isso pode ajudar a garantir que nosso código estará de acordo com as especificações e deve funcionar corretamente.

## Veja também

- [Documentação oficial de testes em Elm](https://guide.elm-lang.org/testing/)
- [Testes com Elm - Blog da Comunidade Elm no Brasil](https://elmbr.blog/testes-com-elm/)
- [Tutorial de TDD com Elm - Medium](https://medium.com/@tomyitav/elm-tdd-part-1-writing-tests-with-lunch-5f0f2f914678)