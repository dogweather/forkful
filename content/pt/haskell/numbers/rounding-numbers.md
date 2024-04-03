---
date: 2024-01-26 03:44:49.932659-07:00
description: "Arredondar n\xFAmeros significa ajust\xE1-los ao inteiro mais pr\xF3\
  ximo ou \xE0 casa decimal especificada. Programadores arredondam n\xFAmeros para\
  \ controlar precis\xE3o,\u2026"
lastmod: '2024-03-13T22:44:46.620314-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros significa ajust\xE1-los ao inteiro mais pr\xF3ximo\
  \ ou \xE0 casa decimal especificada."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como fazer:
Haskell usa as funções `round`, `ceiling`, `floor` e `truncate` do `Prelude` para operações de arredondamento.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- Arredondar para uma casa decimal específica não está no Prelude.
  -- Aqui está uma função personalizada:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## Aprofundamento
Historicamente, arredondar é significativo na análise numérica e ciência da computação porque é crucial para minimizar a acumulação de erro em computações, particularmente antes das representações de ponto flutuante serem padronizadas com IEEE 754.

Para que arredondar? `round` te leva ao inteiro mais próximo—para cima ou para baixo. `ceiling` e `floor` sempre arredondam para cima ou para baixo até o inteiro mais próximo, respectivamente, enquanto `truncate` simplesmente descarta os pontos decimais.

Alternativas a estas funções podem envolver lógica personalizada, como nosso `roundTo`, ou você pode incorporar bibliotecas (como Data.Fixed) para requisitos mais complexos.

Cuidado com resultados inesperados devido à forma como Haskell lida com casos de meio termo em `round` (ele arredonda para o número par mais próximo).

## Veja Também
- Documentação do Haskell Prelude para funções de arredondamento: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- A Wiki do Haskell sobre aritmética de ponto flutuante: https://wiki.haskell.org/Floating_point_arithmetic
- Norma IEEE 754-2008 para mais informações sobre como o ponto flutuante é tratado em muitas linguagens: https://ieeexplore.ieee.org/document/4610935
