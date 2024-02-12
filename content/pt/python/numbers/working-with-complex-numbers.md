---
title:                "Trabalhando com números complexos"
date:                  2024-01-26T04:44:51.443482-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Números complexos são um conjunto de números do tipo `a + bi`, onde `a` e `b` são números reais, e `i` é a unidade imaginária (`i^2 = -1`). Na programação, nós os usamos para resolver problemas em várias áreas, como engenharia elétrica, processamento de sinais e computação quântica.

## Como fazer:
Python tem suporte incorporado para números complexos. Veja como você pode brincar com eles:

```Python
# Criando números complexos
z = 4 + 5j
print(z)  # Saída: (4+5j)

# Acessando partes real e imaginária
print(z.real)  # Saída: 4.0
print(z.imag)  # Saída: 5.0

# Aritmética complexa
w = 1 - 2j
print(z + w)  # Saída: (5+3j)
print(z - w)  # Saída: (3+7j)
print(z * w)  # Saída: (14+2j)
print(z / w)  # Saída: (-3.6+1.2j)

# Módulo (valor absoluto)
print(abs(z))  # Saída: 6.4031242374328485

# Conjugado de um número complexo
print(z.conjugate())  # Saída: (4-5j)
```

## Aprofundando
Os números complexos foram primeiramente conceituados por Gerolamo Cardano no século 16. Python, entre outras linguagens de programação, trata números complexos como cidadãos de primeira classe. Isso significa que eles estão incorporados na linguagem, com recursos fáceis de usar, evitando a necessidade de importar bibliotecas externas para operações básicas.

No entanto, para cálculos numéricos intensivos, Python tem uma biblioteca chamada `cmath`, que é especificamente para números complexos. Ela possui funções adicionais como `exp`, `log` e operações trigonométricas.

Quando Python não é suficiente, você pode recorrer a bibliotecas como NumPy, especialmente para operações de array envolvendo números complexos. NumPy fornece operações otimizadas e vetorizadas que são cruciais para o desempenho em computação numérica.

## Veja Também
Confira esses recursos para aprender mais:

- Documentação oficial do Python sobre números complexos: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- A documentação do módulo `cmath`: https://docs.python.org/3/library/cmath.html
- NumPy para manipulação de arrays de números complexos: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
