---
title:                "Haskell: Lendo argumentos da linha de comando"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade valiosa para qualquer programador Haskell. Ao entender como processar e manipular esses argumentos, você pode criar programas mais eficientes e flexíveis, permitindo que o usuário personalize a execução do seu código.

## Como fazer a leitura de argumentos da linha de comando em Haskell

Para ler os argumentos da linha de comando em Haskell, você precisa importar o módulo `System.Environment`. Em seguida, use a função `getArgs` para obter uma lista de strings contendo os argumentos passados na chamada do seu programa. Você também pode especificar a quantidade de argumentos esperada usando a função `length`.

Veja um exemplo simples de código que lê dois argumentos da linha de comando e imprime a soma deles:

```Haskell
import System.Environment

main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "Erro: forneça exatamente 2 argumentos"
    else do
      let x = read $ args !! 0 -- converte o primeiro argumento para um número
      let y = read $ args !! 1 -- converte o segundo argumento para um número
      putStrLn $ "A soma de " ++ show x ++ " e " ++ show y ++ " é " ++ show (x + y)
```
Ao executar esse código com `runhaskell soma.hs 3 5`, o output será:

```
A soma de 3 e 5 é 8
```

## Aprofundando na leitura de argumentos da linha de comando

Ao trabalhar com a leitura de argumentos da linha de comando, é importante ter em mente que eles são sempre lidos como strings. Portanto, se você precisar usá-los como outros tipos de dados (como números), é necessário convertê-los usando funções como `read` ou `readMaybe` do módulo `Text.Read`.

Além disso, o primeiro argumento (index 0) é sempre o nome do programa em execução, e os demais argumentos são passados na ordem em que foram especificados.

## Veja também

- [Documentação do módulo System.Environment](https://hackage.haskell.org/package/base/docs/System-Environment.html)
- [Guia básico do Haskell](https://www.haskell.org/tutorial/)