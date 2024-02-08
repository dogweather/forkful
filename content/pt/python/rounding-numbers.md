---
title:                "Arredondamento de números"
aliases:
- pt/python/rounding-numbers.md
date:                  2024-01-26T03:46:14.077472-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/rounding-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Arredondar números significa ajustá-los para que fiquem mais próximos de um valor mais simples ou significativo. Programadores arredondam números para simplificar resultados, limitar casas decimais para exibição ou para determinados fins matemáticos.

## Como Fazer:
Aqui está o essencial sobre arredondamento de números em Python:

```python
# Arredondar um número para o inteiro mais próximo
print(round(8.67))  # Saída: 9

# Arredondar um número para um número especificado de casas decimais
print(round(8.67, 1))  # Saída: 8.7

# Números pares são arredondados para baixo e ímpares são arredondados para cima quando equidistantes
print(round(2.5))  # Saída: 2
print(round(3.5))  # Saída: 4
```

## Aprofundando
Em Python, a função `round()` não está apenas cortando decimais. Historicamente, Python, assim como muitas outras linguagens, segue o "arredondamento pela metade até o par" ou "arredondamento do banqueiro". Isso minimiza o erro cumulativo em somas ou médias, o que é importante em cálculos financeiros.

Para alternativas, você tem `math.floor()` e `math.ceil()` do módulo de matemática do Python, arrastando números para baixo ou para cima até o próximo número inteiro. Mas, se precisão é o que você procura, `quantize()` do módulo `decimal` permite especificar o comportamento de arredondamento.

Por baixo dos panos, `round()` lida com números de ponto flutuante binários. Como alguns decimais não podem ser expressos exatamente em binário, você pode se surpreender com coisas como `round(2.675, 2)` não se tornando `2.68` como esperado. Aí entram `decimal` ou `fractions` para alta precisão.

## Veja Também
- Documentação do Python sobre funções incorporadas: https://docs.python.org/3/library/functions.html#round
- Aritmética de ponto fixo e ponto flutuante decimal: https://docs.python.org/3/library/decimal.html
- Módulo de matemática do Python: https://docs.python.org/3/library/math.html
