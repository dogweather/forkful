---
title:    "Haskell: Gerando números aleatórios"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é útil?

Gerar números aleatórios é uma ferramenta essencial para muitos programadores em Haskell. Com a capacidade de gerar números aleatórios, podemos criar jogos, realizar testes de software e simular processos estocásticos. Além disso, é uma ótima maneira de adicionar variedade e imprevisibilidade aos seus programas.

## Como fazer em Haskell?

Para gerar números aleatórios em Haskell, usamos a função `randomR` do módulo `System.Random`. Esta função recebe dois parâmetros: um intervalo mínimo e máximo e retorna um número aleatório dentro deste intervalo. Aqui está um exemplo de como usá-lo:

```Haskell
import System.Random

main = do
   -- Define o intervalo de 1 a 10
   let min = 1
   let max = 10
   -- Gera um número aleatório
   num <- randomRIO (min, max :: Int)
   -- Imprime o número
   putStrLn $ "O número aleatório é: " ++ show num
```

A saída deste programa pode ser algo como:

```bash
O número aleatório é: 7
```

Além disso, também podemos gerar listas de números aleatórios usando a função `randomRs`, que é semelhante à função `randomR`, mas retorna uma lista de valores aleatórios. Aqui está um exemplo:

```Haskell
import System.Random

main = do
   -- Define o intervalo de 1 a 10
   let min = 1
   let max = 10
   -- Gera uma lista de 5 números aleatórios
   nums <- take 5 . randomRs (min, max) <$> newStdGen
   -- Imprime a lista
   putStrLn $ "A lista de números aleatórios é: " ++ show nums
```

A saída deste programa pode ser algo como:

```bash
A lista de números aleatórios é: [4, 2, 9, 6, 8]
```

## Profundidade no mundo dos números aleatórios

Embora a função `randomR` seja uma maneira simples e eficiente de gerar números aleatórios, ela possui algumas limitações. Por exemplo, os números gerados seguem uma distribuição uniforme, o que significa que cada número tem a mesma probabilidade de ser selecionado. Se quisermos uma distribuição diferente, podemos usar a função `random` e fornecer um gerador de números aleatórios como um parâmetro.

Além disso, podemos usar a função `mkStdGen` para criar geradores de números aleatórios determinísticos, o que significa que cada vez que executarmos o programa, obteremos os mesmos números aleatórios. Isso pode ser útil para depuração e teste.

Há também módulos como `System.Random.Shuffle` que fornecem maneiras mais avançadas de gerar números aleatórios, como embaralhar uma lista aleatoriamente usando uma semente.

## Veja também

- [Documentação do módulo `System.Random`](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Documentação do módulo `System.Random.Shuffle`](https://hackage.haskell.org/package/random-shuffle)
- [Explicação sobre números aleatórios em Haskell](https://wiki.haskell.org/Random_vs._system-random)