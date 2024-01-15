---
title:                "Gerando números aleatórios"
html_title:           "Elixir: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que usar geradores de números aleatórios?

Há várias razões pelas quais gerar números aleatórios pode ser útil em projetos de programação. Algumas das principais razões incluem a necessidade de geração de dados de teste, jogos ou aplicações de sorteio, entre outros. Em geral, geradores de números aleatórios ajudam a adicionar uma camada de imprevisibilidade e diversão aos aplicativos.

## Como fazer isso em Elixir

A linguagem de programação Elixir oferece várias maneiras de gerar números aleatórios. Um dos métodos mais comuns é usando a função `:random.uniform/1`, que retorna um número aleatório entre 0 e 1.

```
iex> :random.uniform() # exemplo de saída: 0.4755417463954648
```

Se quiser gerar um número dentro de um intervalo específico, você pode usar a função `:random.uniform/2`, passando o intervalo desejado como argumento.

```
iex> :random.uniform(1, 10) # exemplo de saída: 6
```

Outro método é usando a biblioteca `:rand` e suas funções `:uniform/0` e `:uniform/1` para gerar números aleatórios em diferentes formatos, como inteiros, floats e strings.

```
iex> :rand.uniform() # exemplo de saída: 0.2076688796336488
iex> :rand.uniform(100) # exemplo de saída: 56
```

É importante lembrar que, ao usar geradores de números aleatórios, é necessário inicializar a semente (seed) para garantir que os resultados sejam realmente aleatórios. Isso pode ser feito usando a função `:rand.seed/1` e passando um número inteiro como argumento.

```
iex> :rand.seed(123)
:ok
```

Agora, ao gerar números aleatórios, eles serão baseados na semente inicializada.

## Aprofundando

É importante lembrar que os geradores de números aleatórios na programação não são realmente aleatórios, mas sim pseudoaleatórios. Ou seja, eles seguem um algoritmo para gerar uma sequência de números que parecem ser aleatórios, mas na verdade são predeterminados.

Por causa disso, é importante escolher uma semente inicial não previsível, como um número randômico baseado no tempo atual ou em eventos imprevisíveis do sistema. Além disso, é recomendado não confiar totalmente em geradores de números aleatórios para fins de criptografia ou segurança, pois é possível prever a sequência gerada se a semente for conhecida.

Para mais informações sobre geradores de números aleatórios em Elixir, consulte a documentação oficial: https://hexdocs.pm/elixir/1.13/Random.html

## Veja também

- https://hexdocs.pm/elixir/1.13/Random.html
- https://elixirschool.com/lessons/basics/random/
- https://www.youtube.com/watch?v=fqos5W4FS4c