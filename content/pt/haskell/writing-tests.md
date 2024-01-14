---
title:                "Haskell: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma prática fundamental em qualquer linguagem de programação, e o Haskell não é exceção. Testes bem escritos garantem que nosso código esteja livre de erros e funcionando corretamente, garantindo assim a qualidade do nosso software. Além disso, testes permitem que tenhamos confiança em fazer alterações e adições em nosso código sem medo de causar problemas em outras partes do programa.

## Como escrever testes em Haskell

Testes em Haskell são escritos usando a biblioteca de testes "HUnit". Primeiro, importamos a biblioteca no nosso código:

```Haskell
import Test.HUnit
```

Em seguida, definimos nossos testes usando a função "TestCase". Aqui está um exemplo de como podemos testar uma função que calcula a área de um círculo:

```Haskell
testArea = TestCase (assertEqual "Área do círculo" 78.54 (areaCirculo 5))

```

Em seguida, agrupamos todos os nossos casos de teste em uma lista e passamos como argumento para a função "runTestTT", que executará todos os testes e imprimirá os resultados para nós:

```Haskell
testes = TestList [testArea, testSoma, testSubtrai]
main = runTestTT testes
```

## Mergulho profundo em escrever testes

Além de escrever testes utilizando a biblioteca "HUnit", também podemos utilizar a biblioteca "QuickCheck" para testes automatizados. Essa biblioteca gera entradas aleatórias para nossas funções e verifica se a saída é consistente. Isso é especialmente útil em casos de funções com muitas entradas possíveis, pois verifica se nosso código é resiliente e capaz de lidar com diferentes cenários.

Além disso, é importante lembrar que testes não devem apenas verificar se o código funciona, mas também devem verificar se ele falha adequadamente quando necessário. Ou seja, também devemos escrever testes para verificar o comportamento de nosso código em casos de erro.

Em resumo, escrever testes é uma prática fundamental e necessária para garantir a qualidade do nosso código em Haskell. Utilize bibliotecas como "HUnit" e "QuickCheck" para criar testes abrangentes e automatizados, e sempre verifique se seu código é resiliente em casos de erro.

## Veja também

- [Documentação oficial do HUnit](https://hackage.haskell.org/package/HUnit)
- [Documentação oficial do QuickCheck](https://hackage.haskell.org/package/QuickCheck)