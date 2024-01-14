---
title:    "Haskell: Iniciando um novo projeto"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que

Começar um novo projeto em Haskell é uma ótima maneira de aprimorar suas habilidades de programação funcional e explorar novas formas de resolver problemas. Além disso, Haskell é uma linguagem altamente expressiva e elegante, o que torna a codificação muito mais divertida e desafiadora.

## Como Fazer

Para começar um novo projeto em Haskell, você precisa ter o compilador GHC instalado no seu computador. Em seguida, crie um arquivo com extensão ".hs" e escreva seu código dentro deste arquivo.

```
Haskell
--Exemplo de código
module Main where

--Função para somar dois números
soma :: Int -> Int -> Int
soma x y = x + y

--Função principal que imprime o resultado da soma
main :: IO ()
main = do
  let resultado = soma 5 10
  putStrLn ("O resultado da soma é: " ++ show resultado)
```

O código acima declara uma função `soma` que recebe dois argumentos do tipo `Int` e retorna a soma desses valores. Em seguida, na função principal, chamamos a função `soma` passando os valores 5 e 10 como argumentos e imprimimos o resultado na tela.

Para compilar e executar o código acima, abra o terminal e navegue até o diretório onde seu arquivo ".hs" está localizado. Em seguida, execute os seguintes comandos:

```
ghc -o meu_programa meu_codigo.hs
./meu_programa
```

Isso irá gerar um arquivo executável chamado "meu_programa" e executá-lo, imprimindo na tela o resultado "O resultado da soma é: 15".

## Mergulho Profundo

Ao começar um novo projeto em Haskell, é importante considerar alguns aspectos, como a estrutura do projeto e as bibliotecas necessárias. Uma boa prática é dividir seu código em módulos e utilizar o sistema de tipos de Haskell para garantir que seu código seja seguro e livre de erros.

Também é importante pesquisar e utilizar bibliotecas de terceiros sempre que possível, pois elas podem economizar tempo e permitir que você se concentre na lógica do seu código.

Outra dica importante é sempre testar seu código de forma exaustiva usando o framework de testes HUnit, que é especialmente projetado para testes de unidade em Haskell.

## Veja Também

- [Documentação oficial do GHC](https://www.haskell.org/ghc/)
- [Aprenda Haskell](https://learn.haskell.org/)
- [Hackage - Repositório de bibliotecas Haskell](https://hackage.haskell.org/)