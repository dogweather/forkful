---
title:                "Haskell: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Se você já trabalhou com programação em Haskell, provavelmente já se deparou com o desafio de encontrar e corrigir erros no seu código. Uma técnica útil para resolver esse problema é imprimir resultados de debug, ou seja, exibir valores de variáveis e execuções de funções para entender melhor o que está acontecendo no seu programa.

## Como Fazer

Em Haskell, é possível imprimir informações no console usando a função `print`, que aceita qualquer tipo de dado como argumento. Por exemplo, se quisermos imprimir o valor de uma variável `x` inteira, podemos fazer isso da seguinte forma:

```Haskell
print x
```
Isso irá exibir o valor de `x` no console. Você também pode usar a função `putStrLn` para imprimir uma string customizada junto com o valor, por exemplo:

```Haskell
let x = 10
putStrLn "O valor de x é:"
print x
```

O resultado no console seria:

```
O valor de x é:
10
```

Além de imprimir valores de variáveis, também é possível usar a função `trace` do módulo `Debug.Trace` para imprimir informações durante a execução de funções. Isso pode ser útil para entender como os valores são passados e alterados dentro de uma função. Veja um exemplo:

```Haskell
import Debug.Trace (trace)

funcao :: Int -> Int
funcao x = trace ("O valor de x é: " ++ show x) (x * 2)
```

Quando a função `funcao` for chamada, o resultado será:

```
O valor de x é: 5
```

Além disso, é possível usar a função `traceShow` do mesmo módulo para imprimir o valor retornado por uma função. Por exemplo:

```Haskell
funcao2 :: Int -> Int
funcao2 x = traceShow (x + 10) (x * 2)
```

Ao chamar a função `funcao2`, o resultado no console será:

```
15
```

## Deep Dive

Além das funções mencionadas acima, o Haskell possui diversas ferramentas para imprimir e analisar informações de debug em tempo de execução. Por exemplo, o pacote `Debug.Pretty.Simple` fornece a função `pPrint` que pode ser usada para imprimir tipos de dados complexos de forma mais visualmente agradável. A integração com o depurador GHCi também é uma opção para investigar problemas em tempo real.

Lembre-se que, apesar de ser uma técnica útil, imprimir debug output em excesso pode deixar o seu código bagunçado e difícil de entender. Por isso, é importante usar essa técnica com moderação e sempre remover o código de debug antes de fazer o deploy do seu app.

## Veja Também

- [Documentação oficial do GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)
- [Pacote Debug.Pretty.Simple](https://hackage.haskell.org/package/pretty-simple)
- [Artigo do Medium sobre depuração em Haskell](https://medium.com/@jarednielsen/a-beginners-guide-to-debugging-haskell-code-bdd71aebaaff)