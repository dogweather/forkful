---
title:                "Haskell: Começando um novo projeto"
simple_title:         "Começando um novo projeto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Haskell?

Se você está procurando uma linguagem de programação funcional que ofereça tipagem estática e segurança de tipos, então Haskell pode ser uma ótima opção para você. Além disso, a sintaxe pura e a forte ênfase em funções imutáveis tornam Haskell uma linguagem poderosa e elegante.

## Como começar um novo projeto em Haskell

Começar um novo projeto em Haskell é relativamente simples. Primeiro, é necessário instalar o compilador GHC e o gerenciador de pacotes Cabal. Em seguida, você pode criar um novo projeto através do comando "cabal init" e, em seguida, adicionar suas dependências ao arquivo "cabal.yaml". Aqui está um exemplo de um projeto Haskell simples que calcula o quadrado de um número:

```Haskell
-- importando a biblioteca para manipulação de números
import Data.Numbers

-- função que calcula o quadrado de um número
square :: Int -> Int
square x = x * x

-- chamando a função e exibindo o resultado
main :: IO ()
main = do
  let num = 5
  let result = square num
  putStrLn ("O quadrado de " ++ show num ++ " é " ++ show result)
```

A saída desse programa seria:

```
O quadrado de 5 é 25
```

## Aprofundando-se

Iniciar um novo projeto em Haskell pode ser um pouco diferente de outras linguagens de programação, já que é uma linguagem funcional pura. Portanto, é importante entender os conceitos de tipos, imutabilidade e recursão. Além disso, você também pode considerar a utilização de algumas bibliotecas comuns, como a biblioteca de parsing "Parsec" ou a biblioteca de manipulação de JSON "Aeson". Não tenha medo de experimentar e explorar o poder da programação funcional em Haskell.

## Veja também

- [Instalando GHC e Cabal](https://www.haskell.org/platform/)
- [Introdução ao Haskell](https://www.tutorialspoint.com/haskell/index.htm)
- [Documentação do Cabal](https://www.haskell.org/cabal/)
- [Biblioteca Parsec](https://hackage.haskell.org/package/parsec)
- [Biblioteca Aeson](https://hackage.haskell.org/package/aeson)

Agora que você entende o básico de iniciar um projeto em Haskell, é hora de começar a criar programas incríveis usando essa poderosa linguagem de programação funcional. Experimente e divirta-se aprendendo mais sobre os recursos do Haskell!