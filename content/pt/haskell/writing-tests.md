---
title:                "Escrevendo testes"
html_title:           "Haskell: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-tests.md"
---

{{< edit_this_page >}}

Todos os programadores sabem que testar seu código é essencial para garantir que ele funcione corretamente e que possíveis erros sejam encontrados antes que o software seja usado. No entanto, nem todos os programadores sabem como escrever testes efetivos. Neste artigo, vamos mostrar como escrever testes em Haskell, a linguagem de programação funcional atualmente mais popular, e por que é importante fazê-lo.

## O que & Por quê?

Escrever testes é um conjunto de técnicas usadas para garantir que o software que você escreve funciona corretamente. Como programadores, queremos ter certeza de que nosso código está funcionando corretamente e que possíveis falhas foram identificadas antes que o software chegue às mãos dos usuários. Testes bem escritos podem ajudar a detectar bugs e erros de lógica que poderiam passar despercebidos durante o desenvolvimento.

## Como fazer:

Para escrever testes em Haskell, primeiro você precisa importar o módulo de teste padrão ```Test.HUnit```. Em seguida, você pode criar uma função de teste usando a função ```TestLabel```, que recebe como argumentos uma String e uma expressão booleana. Esta expressão booleana deve ser uma função que retorne ```True``` quando o teste passar e ```False``` quando falhar.

```Haskell
sumTest = TestCase (assertEqual "Soma incorreta!" 3 (1+2))

tests = TestList [TestLabel "Teste de Soma" sumTest]

main = runTestTT tests
```

O exemplo acima cria um teste simples para garantir que a soma de 1 e 2 seja igual a 3. A função ```TestCase``` envolve o resultado da função ```assertEqual```, que compara o valor esperado com o resultado do cálculo da soma. Em seguida, os testes são reunidos em uma lista usando a função ```TestList```. Finalmente, a função ```runTestTT``` executa os testes e retorna o resultado.

## Detalhando mais:

A prática de escrever testes é conhecida como Test-Driven Development (TDD). Ela surgiu no início dos anos 2000 e tem sido amplamente adotada por muitos programadores por seus benefícios no desenvolvimento de software.

Existem também outras bibliotecas de teste para Haskell, como o QuickCheck, que permitem gerar dados de teste automaticamente.

Os testes em Haskell são escritos seguindo o mesmo estilo de programação funcional, o que permite criar testes mais simples e concisos comparados a outras linguagens.

## Veja também:

-[Documentação do módulo de teste HUnit](https://hackage.haskell.org/package/HUnit)
-[Introdução ao TDD em Haskell](https://wiki.haskell.org/Test-driven_development)
-[QuickCheck: propriedades estáticas em Haskell](https://hackage.haskell.org/package/QuickCheck)