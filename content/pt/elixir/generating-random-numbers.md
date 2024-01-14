---
title:    "Elixir: Gerando números aleatórios"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que utilizar a geração de números aleatórios em Elixir?

Gerar números aleatórios é uma necessidade comum em muitos programas e aplicações. Isso pode ser útil para criar dados de teste, jogos, sorteios, entre outras situações. Em Elixir, temos várias funções nativas para gerar números aleatórios, tornando essa tarefa muito simples e eficiente.

## Como fazer em Elixir?

Podemos utilizar a função `:random.uniform()` para gerar um número aleatório dentro de um determinado intervalo. Por exemplo, se quisermos gerar um número entre 1 e 10, podemos fazer o seguinte:

```Elixir
num = :random.uniform(1..10)
IO.puts "O número gerado é #{num}"
```

A saída seria algo semelhante a: `O número gerado é 7`. Além disso, também temos as funções `:random.seed()` e `:random.seed_srand()` para definir uma semente e garantir que os números gerados sejam verdadeiramente aleatórios.

## Mergulhando mais fundo

Por trás dessas funções, Elixir utiliza o algoritmo Mersenne Twister para gerar números aleatórios. Esse algoritmo é considerado muito eficiente e confiável.

É importante notar que esses números não são completamente aleatórios, mas sim "pseudo-aleatórios". Isso significa que, se utilizarmos a mesma semente, sempre obteremos a mesma sequência de números aleatórios. Portanto, é recomendável utilizar uma semente diferente para cada execução do programa.

## Veja também

- [Documentação oficial sobre geração de números aleatórios em Elixir](https://hexdocs.pm/elixir/Random.html)
- [Artigo sobre algoritmos de geração de números aleatórios](https://en.wikipedia.org/wiki/Mersenne_Twister)