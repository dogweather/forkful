---
date: 2024-01-26 03:50:20.611670-07:00
description: "Vamos passear com o GHCi, o ambiente interativo do Haskell que pode\
  \ atuar como um depurador b\xE1sico. Voc\xEA o inicia com o seu c\xF3digo Haskell\
  \ e come\xE7a a\u2026"
lastmod: '2024-03-13T22:44:46.629715-06:00'
model: gpt-4-0125-preview
summary: "Vamos passear com o GHCi, o ambiente interativo do Haskell que pode atuar\
  \ como um depurador b\xE1sico. Voc\xEA o inicia com o seu c\xF3digo Haskell e come\xE7\
  a a\u2026"
title: Usando um depurador
weight: 35
---

## Como fazer:
Vamos passear com o GHCi, o ambiente interativo do Haskell que pode atuar como um depurador básico. Você o inicia com o seu código Haskell e começa a explorar. Aqui está um exemplo:

```Haskell
main :: IO ()
main = do
    putStrLn "Ei, qual é o seu nome?"
    name <- getLine
    putStrLn $ "Olá, " ++ name ++ "! Vamos depurar."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- Finja que há um bug aqui
```

Para começar a depurar com o GHCi:

```bash
$ ghci SeuArquivoHaskell.hs
```

Define um ponto de interrupção em `buggyFunction`:

```Haskell
Prelude> :break buggyFunction
```

Execute seu programa:

```Haskell
Prelude> :main
Ei, qual é o seu nome?
```

Seu programa pausa em `buggyFunction`. Agora você pode inspecionar variáveis, avançar pelo código e avaliar expressões.

## Mergulho Profundo:
Historicamente, a reputação do Haskell por funções puras e tipagem forte levou à crença de que ferramentas de depuração eram menos críticas. A realidade é diferente—programas complexos sempre se beneficiam de boas ferramentas de depuração. O GHCi fornece comandos básicos de depuração. No entanto, para uma experiência mais visual ou aplicações em maior escala, você pode explorar IDEs com depuradores integrados, como o Visual Studio Code com extensões para Haskell ou o plugin Haskell do IntelliJ.

Alternativas ao depurador incluem usar declarações de impressão, conhecidas como "depuração printf," ou aproveitar o sistema de tipos forte do Haskell para tornar estados incorretos irrepresentáveis. Ainda assim, às vezes nada substitui o passo a passo pelo código.

Quanto aos detalhes de implementação, o depurador do Haskell trabalha com o sistema de tempo de execução. Ele pode lidar com pontos de interrupção, execução passo a passo, e permitir a inspeção de variáveis. No entanto, como o Haskell é avaliado preguiçosamente, as coisas podem se tornar um pouco contra-intuitivas. Depurar um programa Haskell muitas vezes significa ficar de olho em quando e como as expressões são avaliadas.

## Veja Também:
- [Guia do Usuário GHC - Depurador](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [Plugin Haskell do IntelliJ](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
