---
title:    "Haskell: Imprimindo saída de depuração"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Se você já esteve envolvido em projetos de programação, provavelmente já se deparou com bugs e problemas que precisam ser resolvidos. Uma das formas de encontrar a origem desses problemas é através da impressão de saída de depuração, permitindo que você visualize o estado do seu programa em um determinado ponto. Esta técnica pode ser muito útil para entender o fluxo de execução do código e identificar possíveis erros.

## Como Fazer

Em Haskell, a impressão de saída de depuração é realizada através da função `print`, que pertence ao módulo `Prelude`. Ela recebe um valor e o imprime na saída padrão. Por exemplo:

```Haskell
> print "Olá mundo!"
"Olá mundo!"
```

Além disso, é possível utilizar a função `putStrLn`, que também pertence ao `Prelude`, para imprimir uma string e quebrar a linha automaticamente. Veja um exemplo:

```Haskell
> putStrLn "Olá"
Olá
```

Outra forma de imprimir saída de depuração é utilizando a função `trace`, do módulo `Debug.Trace`. Esta função é muito útil pois permite que você imprima o valor de uma expressão sem alterar o fluxo de execução do seu código. Veja um exemplo:

```Haskell
> import Debug.Trace
> let x = 10
> trace "Imprimindo o valor de x" x
10
```

## Mergulho Profundo

Ao imprimir saída de depuração, é importante ter em mente algumas boas práticas. Primeiramente, lembre-se de remover todas as impressões de saída antes de finalizar o seu código. Além disso, é recomendado utilizar ferramentas de depuração, como o GHCi ou o GHCi debugger, para evitar a poluição do seu código com `trace`.

É importante também tomar cuidado com a quantidade de saída de depuração que está sendo gerada, pois muitas impressões podem tornar a leitura e entendimento do código mais difícil.

## Veja Também

- [Documentação da função `print` - Haskell Wiki](https://wiki.haskell.org/Printing)
- [Documentação da função `putStrLn` - Haskell Haddock](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:putStrLn)
- [Documentação da função `trace` - Haskell Haddock](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html#v:trace)