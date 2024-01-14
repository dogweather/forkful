---
title:    "Haskell: Começando um novo projeto"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Haskell?

Haskell é uma linguagem de programação funcional pura, que oferece uma sintaxe clara e concisa, tornando-a ideal para desenvolvimento de projetos complexos. Além disso, sua forte tipagem e checagem de tipos estáticos garantem uma maior segurança e confiabilidade no código.

## Como começar um novo projeto em Haskell?

Para começar um novo projeto em Haskell, você precisa ter a linguagem instalada em seu computador. Em seguida, você pode criar um novo arquivo com a extensão .hs e começar a escrever seu código. Veja alguns exemplos abaixo:

```Haskell
-- Função que soma dois números inteiros
soma :: Int -> Int -> Int
soma x y = x + y

-- Função que verifica se um número é par
ePar :: Int -> Bool
ePar x = x `mod` 2 == 0

-- Chamando as funções
main = do
    print (soma 2 3) -- Saída: 5
    print (ePar 4) -- Saída: True
```

## Aprofundando no assunto

Para começar um novo projeto em Haskell, é importante ter um bom entendimento de programação funcional e dos conceitos da linguagem, como imutabilidade, expressões lambda e funções de alta ordem. Além disso, é altamente recomendável utilizar ferramentas de gerenciamento de pacotes, como o Cabal, para instalar e gerenciar bibliotecas e dependências em seu projeto.

## Veja também

- [Página oficial do Haskell](https://www.haskell.org/)
- [Tutorial de Haskell para iniciantes](https://haskell.org/pt-br/tutorial/)
- [Documentação do Cabal](https://www.haskell.org/cabal/)