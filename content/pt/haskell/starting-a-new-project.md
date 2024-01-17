---
title:                "Iniciando um novo projeto"
html_title:           "Haskell: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

O Que e Por Que?

Começar um novo projeto é uma das tarefas mais emocionantes e desafiadoras para um programador. Isso significa que você vai criar algo novo, colocar em prática suas habilidades e conhecimentos, e quem sabe até resolver um problema real. Os programadores iniciam novos projetos para explorar ideias, desenvolver soluções e transformar a sua criatividade em realidade.

Como Fazer:

Para começar um novo projeto em Haskell, você precisa ter o compilador GHC instalado no seu computador. Em seguida, crie um novo diretório para o seu projeto e crie um arquivo com a extensão ".hs" dentro dele. Abra esse arquivo com um editor de texto e comece a escrever seu código. Por exemplo:

```Haskell
-- Meu primeiro programa em Haskell

main = putStrLn "Ola, mundo!"
```

Este é um programa simples que irá imprimir "Ola, mundo!" na tela quando for executado. Para executar o código, abra o terminal, navegue até o diretório onde você salvou o arquivo e digite "ghc <nome-do-arquivo>". Isso irá compilar o seu código e gerar um executável. Para executá-lo, digite "./<nome-do-executavel>".

Se você quiser criar um projeto mais complexo, com vários arquivos, é possível fazer isso utilizando o comando "ghc --make" seguido dos nomes dos arquivos que você quer compilar.

Para ficar mais à vontade com a linguagem, é recomendado que você faça o tutorial "Learn You a Haskell for Great Good! (http://learnyouahaskell.com/)", que irá te ensinar os fundamentos da linguagem.

Mergulho Profundo:

Haskell é uma linguagem funcional pura criada em 1990 pelo cientista da computação Haskell Curry. Ela se diferencia de outras linguagens por utilizar a avaliação preguiçosa e por ser uma linguagem fortemente tipada. Outra característica importante é o Sistema de Tipos de Haskell, que permite ao compilador inferir o tipo de dado utilizado em cada parte do código.

Alternativas para começar um novo projeto em Haskell incluem utilizar ferramentas como Stack e Cabal, que facilitam a criação e gerenciamento de projetos em Haskell. Além disso, você também pode utilizar o ambiente de desenvolvimento integrado (IDE) do seu gosto para escrever código em Haskell.

Veja Também:

- Site oficial de Haskell: https://www.haskell.org/
- Documentação do GHC: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/
- Stack: https://docs.haskellstack.org/en/stable/
- Cabal: https://www.haskell.org/cabal/