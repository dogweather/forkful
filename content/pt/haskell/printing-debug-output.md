---
title:                "Saida de depuração de impressão"
html_title:           "Haskell: Saida de depuração de impressão"
simple_title:         "Saida de depuração de impressão"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir debug output?

A impressão de debug output é uma técnica importante para ajudar a entender o que está acontecendo com o seu código. Ao imprimir informações de depuração durante a execução do programa, você pode identificar e corrigir problemas mais facilmente.

## Como Fazer

Para imprimir informações de depuração em Haskell, podemos utilizar a função `Debug.Trace.trace`. Vamos ver um exemplo simples:

```
Haskell
import Debug.Trace

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

main = do
    let n = 5
    print $ trace ("Calculando fatorial de " ++ show n) (fatorial n)
```

Output:

```
Calculando fatorial de 5
120
```

Neste exemplo, utilizamos a função `trace` para imprimir a mensagem "Calculando fatorial de 5" antes de retornar o resultado do fatorial de 5. Isso nos dá uma visão do que está acontecendo dentro da função `fatorial` e nos ajuda a entender melhor o seu comportamento.

Também podemos utilizar o `trace` dentro de uma expressão para imprimir informações específicas em diferentes pontos do código. Por exemplo:

```
Haskell
import Debug.Trace

dobro :: Int -> Int
dobro x = trace ("Calculando dobro de " ++ show x) (x * 2)

main = do
    let a = 2
    let b = 3
    print $ dobro a + dobro b
```

Output:

```
Calculando dobro de 2
Calculando dobro de 3
10
```

Podemos ver que o `trace` é executado duas vezes, uma para cada chamada da função `dobro`. Isso nos permite rastrear o valor de `a` e `b` durante a execução do programa.

## Deep Dive

A função `trace` é definida no módulo `Debug.Trace` e possui o seguinte tipo:

```
Haskell
trace :: String -> a -> a
```

Esta função recebe uma mensagem de debug como uma `String` e retorna o segundo argumento (que pode ser de qualquer tipo) sem modificá-lo. Isso permite que o `trace` seja utilizado em qualquer lugar que aceite uma expressão, seja dentro de uma função ou de uma expressão maior.

Uma coisa importante a se notar é que a mensagem de debug só será impressa se o programa for compilado com a flag `-debug` ou utilizando a função `traceIO` em vez de `trace`. Isso é importante para garantir que as mensagens de debug não serão impressas em um ambiente de produção, mas apenas durante a fase de desenvolvimento.

## Veja também

- [Aprenda Haskell](https://www.aprendahaskell.com.br/)
- [Documentação do Haskell](https://www.haskell.org/documentation/)
- [Módulo `Debug.Trace`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)