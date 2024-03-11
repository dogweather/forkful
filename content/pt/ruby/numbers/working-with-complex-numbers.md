---
date: 2024-01-26 04:45:43.000414-07:00
description: "N\xFAmeros complexos, compostos por uma parte real e uma imagin\xE1\
  ria (como 3+4i), s\xE3o fundamentais na engenharia e na f\xEDsica. Programadores\
  \ trabalham com eles\u2026"
lastmod: '2024-03-11T00:14:20.831561-06:00'
model: gpt-4-0125-preview
summary: "N\xFAmeros complexos, compostos por uma parte real e uma imagin\xE1ria (como\
  \ 3+4i), s\xE3o fundamentais na engenharia e na f\xEDsica. Programadores trabalham\
  \ com eles\u2026"
title: "Trabalhando com n\xFAmeros complexos"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Números complexos, compostos por uma parte real e uma imaginária (como 3+4i), são fundamentais na engenharia e na física. Programadores trabalham com eles em simulações, processamento de sinais e resolução de equações que não se resolvem apenas com números reais.

## Como fazer:
Ruby torna o manuseio de números complexos muito fácil. Você pode criar e manipulá-los usando a classe Complex:

```ruby
require 'complex'

# Criar números complexos
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Operações básicas
soma = c1 + c2               # => (5.0+9.0i)
diferença = c1 - c2          # => (1.0-1.0i)
produto = c1 * c2            # => (-14.0+23.0i)
quociente = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# Conjugado, magnitude e fase
conjugado = c1.conjugate     # => (3.0-4.0i)
magnitude = c1.abs           # => 5.0
fase = c1.phase              # Math.atan2(4, 3) => 0.9272952180016122 radianos

# Métodos específicos para complexos
polar = c1.polar             # => [5.0, 0.9272952180016122]
retangular = c1.rect         # => [3.0, 4.0]
```

## Aprofundando
Números complexos não são novidade — eles existem desde o século 16, resolvendo equações sem soluções reais. Além da matemática, computacionalmente, a classe Complex do Ruby faz o trabalho pesado, apoiada pelo módulo Math para funções trigonométricas e transcendentes.

Linguagens de programação mais antigas exigiam o manuseio manual das partes real e imaginária. Algumas, como Fortran e C++, dedicam bibliotecas especiais para a aritmética complexa.

A abordagem do Ruby incorpora suporte a números complexos em sua sintaxe, libertando-o da necessidade de reinventar a roda. Nos bastidores, a classe Complex lida com a matemática, enquanto o Ruby cuida das interações entre objetos.

## Veja Também
- Documentação do Ruby sobre Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- Visão da MathWorld sobre Números Complexos: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Uma introdução visual aos números complexos e por que são úteis: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
