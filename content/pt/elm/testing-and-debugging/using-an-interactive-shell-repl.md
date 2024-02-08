---
title:                "Usando um shell interativo (REPL)"
date:                  2024-01-26T04:13:35.598191-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
O Laço Ler-Avaliar-Imprimir (REPL, na sigla em inglês) é um ambiente de programação interativo e simples que recebe entradas únicas do usuário, avalia-as e retorna o resultado ao usuário. Programadores de Elm usam o REPL para experimentos rápidos, depuração ou aprendizado da linguagem.

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
