---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Imprimir a saída de depuração é um meio pelo qual os programadores rastreiam e corrigem problemas em seu código. Esse processo auxilia na identificação de partes específicas do código que não funcionam conforme o esperado.

## Como fazer:

Aqui está um pequeno exemplo de como imprimir a saída de depuração em Haskell com a função `debug`.

```Haskell
import Debug.Trace

main = print $ debug "Valor de x :" (2+2)

debug msg v = trace (msg ++ " " ++ show v) v
```

Neste exemplo, estamos importando a função `trace` do módulo `Debug.Trace` e criando uma função denominada `debug` que toma uma mensagem e um valor. Concatena a mensagem com o valor e o imprime na saída.

## Aprofundando

Imprimir a saída de depuração é prática comum na programação desde os primeiros dias do desenvolvimento de software. Enquanto em Haskell, tendemos a evitar efeitos colaterais e manter a pureza funcional, existem certos módulos como `Debug.Trace` que fornecem funcionalidades imperativas, como imprimir para a saída padrão, o que nos ajuda na depuração da mesma maneira que em outras linguagens imperativas.

Existem alternativas à função `trace`. Outras funções como `traceShow`, `traceShowId ` e `traceShowM` são variações da função `trace` que podem ser usadas dependendo das necessidades específicas.

A função `trace` funciona enviando mensagens de debug para a saída de erro padrão (stderr), e não para a saída padrão (stdout). É importante ter isso em mente ao depurar programas em Haskell.

## Veja também:

Além do `Debug.Trace`, existem outros pacotes disponíveis para depuração em Haskell. Um deles é o `Debug.SimpleReflect` que fornece funções para 'visualizar' expressões. Você pode encontrar mais informações sobre este pacote na [documentação do Hackage](https://hackage.haskell.org/package/simple-reflect).

Outra ótima ferramenta para ajudar na depuração de programas Haskell é o GHCi, o ambiente interativo do GHC. Informações mais detalhadas sobre como usar o GHCi para depuração podem ser encontradas na [documentação do GHC](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-debug).