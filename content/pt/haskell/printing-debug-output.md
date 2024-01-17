---
title:                "Imprimindo saída de depuração"
html_title:           "Haskell: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que e Por quê?

Impressão de saída de depuração é o processo de mostrar mensagens, variáveis ​​e outros dados durante a execução de um programa. Os programadores fazem isso para verificar o estado do programa e detectar possíveis erros.

## Como fazer:

```
-- Exemplo 1:
debug :: String -> IO ()
debug msg = putStrLn ("DEBUG: " ++ msg)

main :: IO ()
main = do
  let x = 5
  debug ("O valor de x é " ++ show x)
  putStrLn ("O dobro de x é " ++ show (x*2))

-- Saída:
DEBUG: O valor de x é 5
O dobro de x é 10

-- Exemplo 2:
import Debug.Trace

main :: IO ()
main = do
  let list = [1,2,3,4,5]
  debug ("A lista original é " ++ show list)
  let newList = trace "Mapeando a lista..." (map (+1) list)
  debug ("A nova lista é " ++ show newList)

-- Saída:
DEBUG: A lista original é [1,2,3,4,5]
Mapeando a lista...
DEBUG: A nova lista é [2,3,4,5,6]
```

## Deep Dive:

A impressão de saída de depuração tem sido usada por muito tempo pelos programadores como uma maneira rápida e fácil de verificar o estado de um programa durante a execução. No entanto, em casos mais complexos, pode ser necessário utilizar uma ferramenta de depuração mais avançada. Algumas alternativas populares incluem o GHCi e o GNU Debugger (GDB). Além disso, é importante ter cuidado ao utilizar a impressão de saída de depuração em código de produção, pois pode prejudicar o desempenho da aplicação.

## Veja também:

- [Documentação oficial do GHC](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [Tutorial de depuração em GHCi](https://www.fpcomplete.com/blog/step-by-step-guide-to-debugging-haskell-in-ghci)
- [Tutorial de depuração com GDB](https://www.tutorialspoint.com/haskell/haskell_debugging.htm)