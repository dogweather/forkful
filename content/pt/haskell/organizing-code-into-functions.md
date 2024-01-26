---
title:                "Organizando o código em funções"
date:                  2024-01-26T01:10:24.157999-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Quê e Porquê?
Organizar código em funções em Haskell significa decompor seu código em blocos nomeados reutilizáveis. Por quê? Isso mantém seu código DRY (Don't Repeat Yourself - Não Se Repita), torna-o legível e mais fácil de depurar.

## Como Fazer:
Eis como você pode escrever e usar funções em Haskell:

```Haskell
-- Definindo uma função simples para adicionar dois números
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Utilizando a função
main = print (addNumbers 3 5)
```

Saída:
```
8
```

Você também pode criar funções de ordem superior:

```Haskell
-- Pega uma função e a aplica duas vezes em algo
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Utilizando applyTwice com uma função anônima
main = print (applyTwice (*2) 5)
```

Saída:
```
20
```

## Aprofundando
Haskell, uma linguagem puramente funcional, trata funções como cidadãos de primeira classe. Historicamente, isso tem raízes no cálculo lambda, uma estrutura fundamental na computação. Diferentemente das linguagens imperativas, onde funções são uma sequência de instruções, em Haskell, funções são expressões que descrevem relações entre dados.

Existem alternativas para escrever funções puras para reutilização. Considere usar typeclasses para polimorfismo ou aproveitar módulos para agrupar funções relacionadas. A avaliação preguiçosa de Haskell também impacta na implementação das funções — as funções não serão avaliadas até que seus resultados sejam necessários, potencialmente afetando considerações de desempenho.

## Veja Também
- Documentação Oficial do Haskell: https://www.haskell.org/documentation/
- "Aprenda Haskell para o Maior Bem!" por Miran Lipovača, um livro amigável para iniciantes: http://learnyouahaskell.com/
- "Haskell do Mundo Real" por Bryan O'Sullivan, Don Stewart e John Goerzen: http://book.realworldhaskell.org/