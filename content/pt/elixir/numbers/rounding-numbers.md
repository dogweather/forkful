---
date: 2024-01-26 03:43:59.579896-07:00
description: "Como Fazer: No Elixir, voc\xEA pode usar `Float.round/2` para arredondar\
  \ um n\xFAmero de ponto flutuante. Voc\xEA pode especificar o n\xFAmero de d\xED\
  gitos decimais que\u2026"
lastmod: '2024-03-13T22:44:46.233220-06:00'
model: gpt-4-0125-preview
summary: "No Elixir, voc\xEA pode usar `Float.round/2` para arredondar um n\xFAmero\
  \ de ponto flutuante."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como Fazer:
No Elixir, você pode usar `Float.round/2` para arredondar um número de ponto flutuante. Você pode especificar o número de dígitos decimais que deseja manter. Veja como funciona:

```elixir
# Arredondar um número para nenhum lugar decimal
Float.round(3.14159) # => 3.0

# Arredondar um número para 2 lugares decimais
Float.round(3.14159, 2) # => 3.14

# Arredondar um número para uma precisão negativa, para a dezena mais próxima
Float.round(123.456, -1) # => 120.0
```

## Aprofundando
Arredondar números é um problema clássico na ciência da computação — tanto que a escolha da estratégia de arredondamento pode impactar sistemas financeiros, cálculos científicos e mais. O `Float.round/2` do Elixir usa por padrão o arredondamento "para cima", assemelhando-se ao arredondamento tradicional ensinado nas aulas de matemática.

Se você precisar de outros tipos de arredondamento, o Elixir permite que você crie o seu próprio. Considere, por exemplo, o arredondamento "para baixo" (sempre para baixo) ou o arredondamento "para cima" (sempre para cima). Você usaria `Float.floor/1` ou `Float.ceil/1`, respectivamente.

```elixir
# Arredondamento para baixo
Float.floor(3.999) # => 3.0

# Arredondamento para cima
Float.ceil(3.001) # => 4.0
```

Essas alternativas ajudam a adaptar o arredondamento às necessidades exatas de sua aplicação, seja ela cálculos financeiros, renderização gráfica ou aproximação de dados.

## Veja Também
Para mais informações sobre as funções de arredondamento e números de ponto flutuante no Elixir:

- Documentação oficial do Elixir sobre `Float`: https://hexdocs.pm/elixir/Float.html
- Padrão IEEE para Aritmética de Ponto Flutuante (IEEE 754): https://ieeexplore.ieee.org/document/4610935
