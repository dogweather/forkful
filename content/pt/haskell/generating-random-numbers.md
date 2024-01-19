---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Gerar números aleatórios envolve a criação de números que não seguem um padrão, mas aparecem ou mudam 'ao acaso'. Programadores usam números aleatórios para tudo, de criar jogos a simular eventos do mundo real.

## Como Fazer:

No Haskell, você pode usar a biblioteca System.Random para gerar números aleatórios. Aqui está um exemplo de código para mostrar como criar um número aleatório entre 1 e 10:

```Haskell
import System.Random

main = do
    gen <- newStdGen
    let (valor, _) = randomR (1, 10) :: (Int, StdGen)
    print valor
```

Quando você executa este programa, ele irá imprimir um número aleatório entre 1 e 10.

## Aprofundamento

Historicamente, a geração de números aleatórios começou com habilidades manuais, como jogar dados ou sorteio de papeis. Na programação, recorremos a algoritmos para criar esses números 'aleatórios', que na verdade são pseudoaleatórios, pois seguem um algoritmo determinístico.

Existem várias maneiras de gerar números aleatórios em Haskell além da randomR. Outros exemplos incluem a função `random` que gera um valor aleatório sem limitações e a função `randoms`, que gera uma lista infinita de números aleatórios.

Os números aleatórios em Haskell são gerados usando um gerador de números aleatórios (`StdGen`). A função `newStdGen` cria uma nova instância de `StdGen` que pode ser usada para gerar números aleatórios.

## Veja Também

Para mais detalhes sobre a biblioteca System.Random, consulte a documentação oficial do Haskell no seguinte link:
[Documentação System.Random](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html)

Para uma discussão mais profunda sobre geração de números aleatórios e pseudoaleatórios, você pode consultar este link:
[Números Aleatórios e Pseudoaleatórios](https://www.scielo.br/scielo.php?script=sci_arttext&pid=S0103-97332006000200002&lng=en&nrm=iso&tlng=pt)