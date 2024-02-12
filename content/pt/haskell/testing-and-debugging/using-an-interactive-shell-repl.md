---
title:                "Usando um shell interativo (REPL)"
aliases:
- /pt/haskell/using-an-interactive-shell-repl/
date:                  2024-01-26T04:14:57.902357-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um shell interativo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
Um shell interativo, ou REPL (Read-Eval-Print Loop), no Haskell permite executar trechos de código ao vivo. É um espaço de experimentação para feedback rápido, testar funções e aprender a linguagem.

## Como fazer:
Para iniciar o GHCi (ambiente interativo do Glasgow Haskell Compiler), basta digitar `ghci` no seu terminal. Veja como usá-lo:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

A saída do exemplo explica que `x` é uma variável numérica e mostra que dobrá-la resulta em 10.

## Aprofundando:
O GHCi do Haskell evoluiu muito desde sua criação. Ele oferece um conjunto rico de recursos, como conclusão de tabulação, entrada de múltiplas linhas e carregamento de pacotes. Alternativas como o Hugs são, na maior parte, históricas agora, sendo o GHCi o padrão. O GHCi compila o código em tempo real toda vez que você insere uma expressão, oferecendo uma maneira eficiente de testar seu código Haskell.

## Veja também:
- [O Guia do Usuário do GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Aprenda You a Haskell for Great Good! – Começando](http://learnyouahaskell.com/starting-out#hello-world)
- [Wiki do Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
