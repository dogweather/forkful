---
date: 2024-01-26 04:13:35.598191-07:00
description: "Como Fazer: Elm n\xE3o vem com um REPL integrado. No entanto, voc\xEA\
  \ pode usar `elm repl` a partir da linha de comando para iniciar uma sess\xE3o Elm\
  \ ap\xF3s\u2026"
lastmod: '2024-03-13T22:44:46.501659-06:00'
model: gpt-4-0125-preview
summary: "Elm n\xE3o vem com um REPL integrado."
title: Usando um shell interativo (REPL)
weight: 34
---

## Como Fazer:
Elm não vem com um REPL integrado. No entanto, você pode usar `elm repl` a partir da linha de comando para iniciar uma sessão Elm após instalar o Elm.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

Nesta sessão, após importar as funções de List, nós dobramos os números em uma lista e obtivemos o resultado instantaneamente.

## Aprofundamento
O REPL de Elm pode parecer limitado em comparação com os de algumas outras linguagens como Python ou JavaScript, uma vez que Elm é uma linguagem compilada focada na produção de aplicativos web. Historicamente, Elm tem se concentrado em aplicações completas em vez de scripts ou interações com o shell.

Alternativas ao REPL de Elm incluem `elm-live` e editores online como o Ellie, onde você pode ver as alterações no código refletidas em tempo real em um navegador.

Em termos de implementação, o REPL de Elm compila trechos do código Elm em JavaScript em segundo plano, permitindo que você execute Elm de forma interativa. Isso é diferente dos REPLs de linguagens interpretadas, que não necessitam dessa etapa de compilação. O REPL de Elm também é simplificado para manter a linguagem principal leve e focada.

## Veja Também
- Guia oficial do Elm sobre interatividade: https://guide.elm-lang.org/interop/
- Ellie, um playground online de Elm: https://ellie-app.com/new
- `elm-live`, um servidor de desenvolvimento flexível para Elm: https://www.elm-live.com/
