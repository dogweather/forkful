---
title:                "Trabalhando com números complexos"
date:                  2024-01-26T04:39:31.135489-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"

category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Números complexos são uma combinação de números reais e imaginários, como `a + bi`, onde `i` é a raiz quadrada de -1. Eles são essenciais em campos como engenharia e física para resolver problemas que números regulares não conseguem.

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
