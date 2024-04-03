---
date: 2024-01-26 04:18:07.720114-07:00
description: "Usar um shell interativo, ou um Loop de Leitura-Avalia\xE7\xE3o-Impress\xE3\
  o (REPL), permite programar interativamente. Programadores usam isso para testar\
  \ trechos\u2026"
lastmod: '2024-03-13T22:44:46.922027-06:00'
model: gpt-4-0125-preview
summary: "Usar um shell interativo, ou um Loop de Leitura-Avalia\xE7\xE3o-Impress\xE3\
  o (REPL), permite programar interativamente."
title: Usando um shell interativo (REPL)
weight: 34
---

## Como fazer:
Invoke o REPL abrindo um terminal e executando `swift`. Digite código diretamente e pressione Enter para rodá-lo. Aqui vai um gostinho:

```Swift
1> let greeting = "Olá, REPL!"
greeting: String = "Olá, REPL!"
2> print(greeting)
Olá, REPL!
```

Saia com `:quit` ou `Control-D`.

## Aprofundamento
As raízes do REPL remontam aos interpretadores Lisp nos anos 60. O REPL do Swift fica no topo do LLVM, um poderoso framework de compilador, oferecendo mais do que apenas interpretação básica - é uma ferramenta completa com auto-completar, depuração e mais. O REPL é ótimo para aprender ou para prototipagem, mas não é um ambiente de desenvolvimento independente. Algumas pessoas preferem usar os Playgrounds no Xcode para uma abordagem mais gráfica e baseada em arquivos, enquanto outras continuam com a edição e execução de scripts tradicionais.

Por debaixo dos panos, o REPL do Swift compila dinamicamente o código para a linguagem de máquina e o executa, e é por isso que é relativamente rápido. Ele também pode acessar quaisquer módulos Swift compilados, ou até bibliotecas C, tornando-o bastante poderoso. Note, no entanto, que nem tudo funciona perfeitamente no REPL; algumas funcionalidades do Swift, especialmente aquelas que requerem configurações de projeto complexas ou arquivos de storyboard, não vão funcionar aqui.

## Veja Também
- [Swift.org - Primeiros Passos](https://www.swift.org/getting-started/#using-the-repl)
- [Introdução aos Playgrounds do Xcode](https://developer.apple.com/videos/play/wwdc2014/408/) da Apple
- [Projeto LLVM](https://llvm.org/)
