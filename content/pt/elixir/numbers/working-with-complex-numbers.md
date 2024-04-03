---
date: 2024-01-26 04:39:14.856685-07:00
description: "Como fazer: O Elixir n\xE3o possui n\xFAmeros complexos incorporados,\
  \ ent\xE3o criamos os nossos ou usamos uma biblioteca, como a `ComplexNum`. Aqui\
  \ est\xE1 um\u2026"
lastmod: '2024-03-13T22:44:46.232281-06:00'
model: gpt-4-0125-preview
summary: "O Elixir n\xE3o possui n\xFAmeros complexos incorporados, ent\xE3o criamos\
  \ os nossos ou usamos uma biblioteca, como a `ComplexNum`."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## Como fazer:
O Elixir não possui números complexos incorporados, então criamos os nossos ou usamos uma biblioteca, como a `ComplexNum`. Aqui está um exemplo rápido com uma biblioteca:

```elixir
# Assumindo que você tenha ComplexNum instalado
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Criar números complexos e adicioná-los
c1 = {3, 4}   # representa 3 + 4i
c2 = {2, -3}  # representa 2 - 3i
resultado = ComplexMath.add(c1, c2)
IO.puts "O resultado é: #{inspect(resultado)}"
```

Isso produziria:
```
O resultado é: {5, 1}
```

Significa que a soma de `3 + 4i` e `2 - 3i` é `5 + 1i`.

## Aprofundando
Números complexos surgiram na história porque os números regulares não podiam lidar com raízes quadradas de negativos. Foi só no século 17 que eles foram levados a sério, graças a matemáticos como René Descartes e Gerolamo Cardano.

No Elixir, você frequentemente usa tuplas como `{3, 4}` para números complexos, ou usa uma biblioteca dedicada para evitar reinventar a roda. Bibliotecas são geralmente melhores - elas lidam com detalhes complicados como multiplicação e divisão, que ficam complicados por causa da unidade imaginária 'i' (FYI: `i` ao quadrado é igual a `-1`).

## Veja Também
Confira esses recursos:
- [Biblioteca ComplexNum](https://hex.pm/packages/complex_num) para o gerenciador de pacotes do Elixir, Hex.
- [Elixir School](https://elixirschool.com/en/), para tópicos avançados e exercícios de Elixir.
- [Erlang -- módulo math](http://erlang.org/doc/man/math.html), que o Elixir usa por baixo dos panos, para outras necessidades matemáticas.
