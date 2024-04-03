---
date: 2024-01-27 20:33:19.251096-07:00
description: "Gerar n\xFAmeros aleat\xF3rios em Elixir \xE9 uma tarefa de programa\xE7\
  \xE3o fundamental, vital para aplica\xE7\xF5es que necessitam de resultados imprevis\xED\
  veis, como na\u2026"
lastmod: '2024-03-13T22:44:46.234187-06:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios em Elixir \xE9 uma tarefa de programa\xE7\
  \xE3o fundamental, vital para aplica\xE7\xF5es que necessitam de resultados imprevis\xED\
  veis, como na gera\xE7\xE3o de tokens seguros, amostragem de dados ou em algoritmos\
  \ de jogos."
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
weight: 12
---

## O Quê & Porquê?

Gerar números aleatórios em Elixir é uma tarefa de programação fundamental, vital para aplicações que necessitam de resultados imprevisíveis, como na geração de tokens seguros, amostragem de dados ou em algoritmos de jogos. Programadores usam isso para introduzir um nível de aleatoriedade e variabilidade em suas aplicações, tornando-as mais dinâmicas e menos determinísticas.

## Como fazer:

Para gerar números aleatórios em Elixir, você utiliza principalmente o módulo `:rand`, que fornece várias funções para este propósito. Aqui está um guia rápido para começar:

Primeiro, garanta que você semeie o gerador de números aleatórios para inicializá-lo com um ponto de partida único:

```elixir
:rand.seed(:exsplus)
```

Para gerar um inteiro aleatório dentro de um intervalo, use:

```elixir
random_integer = :rand.uniform(10) # Gera um número entre 1 e 10
IO.puts(random_integer)
```

Para um float aleatório entre 0 e 1.0:

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

Você pode precisar de um intervalo mais específico para floats, o que requer um pouco mais de cálculo:

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

Lembre-se, esses números são pseudoaleatórios; eles são determinados pela semente e algoritmo, mas suficientes para a maioria das aplicações.

## Aprofundando

As capacidades de geração de números aleatórios de Elixir dependem do módulo `:rand` de Erlang, refletindo sua herança e estreita relação com Erlang. O módulo `:rand` substituiu o mais antigo módulo `:random`, oferecendo algoritmos melhorados para a geração de números aleatórios. Ele fornece uma variedade de algoritmos, sendo o padrão `exsplus`, mas também suporta outros como `exs64`, `exsl`, e mais, cada um com seus compromissos em termos de velocidade e qualidade de aleatoriedade.

Um aspecto interessante da geração de números aleatórios de Elixir (e, portanto, Erlang) é o seu manejo de sementes. O sistema mantém estados de semente separados para cada processo, garantindo que processos concorrentes não interfiram nas sequências de números aleatórios um do outro. Isso é particularmente útil em aplicações concorrentes, assegurando previsibilidade e confiabilidade em sistemas distribuídos.

Enquanto o módulo `:rand` é suficiente para a maioria dos casos de uso, aplicações que requerem números aleatórios criptograficamente seguros devem considerar outras opções. O módulo `crypto` fornece funções como `crypto:strong_rand_bytes/1` que são projetadas para gerar dados aleatórios seguros adequados para fins criptográficos. Essas alternativas são essenciais para aplicações sensíveis à segurança como geração de tokens, criptografia e certos tipos de mecanismos de autenticação.
