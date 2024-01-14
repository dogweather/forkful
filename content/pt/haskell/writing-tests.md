---
title:                "Haskell: Escrevendo testes"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Haskell?

Escrever testes é uma parte crucial do processo de desenvolvimento de software em qualquer linguagem de programação. No entanto, em Haskell, essa prática se torna ainda mais importante devido à sua natureza funcional e forte tipagem de dados. Ter um bom conjunto de testes garante maior confiabilidade do código e capacidade de fazer alterações sem medo de quebrar funcionalidades previamente implementadas.

## Como escrever testes em Haskell?

Felizmente, escrever testes em Haskell é bastante simples. Primeiro, precisamos importar o módulo de teste, que pode ser feito com a seguinte declaração:

```Haskell
import Test.HUnit
```

Agora podemos definir nossos testes com a função `Test` fornecida pelo módulo de teste. Por exemplo, se quisermos testar uma função `sum` para verificar se ela retorna a soma correta de dois números, poderíamos escrever o seguinte teste:

```Haskell
testeSoma = TestCase (assertEqual "2 + 2 deve ser igual a 4" (sum 2 2) 4)
```

Nesse caso, estamos criando um teste com o nome `testeSoma` que verifica se a função `sum` retorna o valor esperado (4) quando passamos 2 e 2 como argumentos. Podemos criar quantos testes quisermos dessa forma e depois agrupá-los em uma lista utilizando a função `TestList`:

```Haskell
testes = TestList [testeSoma, testeOutraFuncao]
```

Agora basta chamar a função `runTestTT` passando nossa lista de testes como argumento para executá-los e ver os resultados:

```Haskell
main = runTestTT testes
```

Se todos os testes passarem, veremos uma mensagem de sucesso. Porém, se algum teste falhar, receberemos informações detalhadas sobre o erro, como a mensagem de teste e os valores atuais e esperados.

## Aprofundando mais nos testes em Haskell

Além da sintaxe básica apresentada acima, o módulo de teste também oferece outras funções úteis, como `assertBool` para testar valores booleanos e `assertString` para testar strings. Também é possível agrupar testes em diferentes contextos utilizando a função `TestLabel` e até mesmo criar testes automáticos utilizando a função `Testable` para gerar testes a partir de uma lista de valores de entrada e saída esperados.

Outra prática comum é utilizar bibliotecas como `QuickCheck` para gerar testes de propriedades automaticamente, ajudando a cobrir uma gama maior de casos de uso.

Lembre-se de que, além de escrever testes, é importante também executá-los regularmente para garantir que o código continue funcionando conforme o esperado, especialmente ao fazer alterações.

## Veja também

- [Documentação oficial do módulo de teste em Haskell](https://hackage.haskell.org/package/HUnit)

- [Tutorial completo de testes em Haskell](https://wiki.haskell.org/HUnit_1.0_Tutorial)