---
date: 2024-01-26 04:39:31.135489-07:00
description: "Como: Elm n\xE3o tem suporte interno para n\xFAmeros complexos, ent\xE3\
  o voc\xEA criar\xE1 seu pr\xF3prio tipo e fun\xE7\xF5es. Aqui est\xE1 uma configura\xE7\
  \xE3o r\xE1pida."
lastmod: '2024-03-13T22:44:46.493951-06:00'
model: gpt-4-0125-preview
summary: "Elm n\xE3o tem suporte interno para n\xFAmeros complexos, ent\xE3o voc\xEA\
  \ criar\xE1 seu pr\xF3prio tipo e fun\xE7\xF5es."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## Como:
Elm não tem suporte interno para números complexos, então você criará seu próprio tipo e funções. Aqui está uma configuração rápida:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Exemplo de uso:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

soma = add a b
-- soma é { real = 4.0, imaginary = -2.0 }
```

## Aprofundamento
Historicamente, os números complexos nem sempre foram aceitos. Eles se tornaram uma mudança de jogo no século 16 para resolver equações cúbicas. Alternativas em outras linguagens como Python oferecem suporte embutido para números complexos com operações prontas para uso. Elm requer uma abordagem faça você mesmo, como você viu. Mas você pode torná-lo tão sofisticado quanto necessário, construindo multiplicação, divisão e outras operações, ajustando questões de desempenho.

## Veja Também
- Documentação Oficial do Elm: https://package.elm-lang.org/ para criar tipos personalizados e dominar os fundamentos do Elm.
- Aficionados por história da matemática podem conferir "An Imaginary Tale" (Uma História Imaginária) de Paul J. Nahin para uma viagem dos números complexos através do tempo.
- Mergulhe em desafios de programação orientados à matemática no Project Euler (https://projecteuler.net) para aplicar sua magia de números complexos.
