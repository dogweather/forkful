---
date: 2024-01-26 00:53:44.666531-07:00
description: "Como fazer: Haskell lida com erros de forma robusta atrav\xE9s de tipos\
  \ como `Maybe` e `Either`. Aqui est\xE1 uma r\xE1pida olhada."
lastmod: '2024-03-13T22:44:46.632515-06:00'
model: gpt-4-1106-preview
summary: "Haskell lida com erros de forma robusta atrav\xE9s de tipos como `Maybe`\
  \ e `Either`."
title: Tratamento de erros
weight: 16
---

## Como fazer:
Haskell lida com erros de forma robusta através de tipos como `Maybe` e `Either`. Aqui está uma rápida olhada:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Dividir por zero é proibido, então retornamos Nothing.
safeDivide x y = Just (x `div` y)  -- Caso contrário, está tudo bem, retornamos o resultado em um Just.

-- Vamos ver isso em ação:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Para um tratamento de erros mais complexo, `Either` entra em jogo:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Erro de divisão por zero."  -- Desta vez, o erro vem com uma mensagem.
safeDivideEither x y = Right (x `div` y)

-- E em uso:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Erro de divisão por zero."
```

## Aprofundando
No mundo Haskell, o tratamento de erros tem uma longa história. Antigamente, erros podiam derrubar seu programa inteiro—nada divertido. O sistema de tipos do Haskell oferece maneiras de tornar isso muito menos provável. Temos `Maybe` e `Either`, mas existem outros como `Exceptions` e `IO` para diferentes cenários.

`Maybe` é simples: você recebe `Just` algo se estiver tudo bem, ou `Nothing` se não estiver. `Either` dá um passo adiante, permitindo retornar uma mensagem de erro (`Left`) ou um resultado bem-sucedido (`Right`).

Ambos são puros, o que significa que não interferem com o mundo exterior – um grande negócio em Haskell. Evitamos as armadilhas de exceções não verificadas que afligem algumas outras linguagens.

Para aqueles que não estão contentes com `Maybe` e `Either`, bibliotecas como `Control.Exception` fornecem tratamento de erros mais tradicionais, no estilo imperativo, através de exceções. Mas usá-las liberalmente pode complicar as coisas, então a comunidade muitas vezes adere aos tipos.

## Veja Também
Aprofunde-se com:

- Documentação própria do Haskell: [Haskell](https://haskell.org/documentation)
- Ótimo para iniciantes: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
