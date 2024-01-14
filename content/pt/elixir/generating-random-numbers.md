---
title:    "Elixir: Gerando números aleatórios"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Elixir?

Gerar números aleatórios é uma necessidade comum em muitos programas e aplicações. Isso pode ser útil para jogos, sorteios, geração de senhas e muitas outras funcionalidades. Em Elixir, essa tarefa é feita de forma eficiente e intuitiva, utilizando a função `rand/1`.

## Como gerar números aleatórios em Elixir

Para gerar um número aleatório em Elixir, utilizamos a função `rand/1`. Essa função pode receber como argumento um número inteiro que representa o intervalo de valores a serem gerados. Por exemplo, se quisermos gerar um número aleatório entre 1 e 10, utilizamos `rand(1..10)`.

Podemos também gerar uma lista de números aleatórios utilizando `Enum.map/2` e `rand/2`. O primeiro argumento é a lista que queremos percorrer e o segundo é a função que será aplicada a cada elemento.

```Elixir
lista = [1, 2, 3, 4, 5]
Enum.map(lista, fn x -> rand(x..10) end)
```

A função `random/0` também pode ser utilizada para gerar números aleatórios entre 0 e 1.

```Elixir
random()
```

O resultado será um número decimal com valor entre 0 e 1.

## Profundizando na geração de números aleatórios

Elixir utiliza o gerador de números aleatórios Mersenne Twister, que é conhecido pela sua eficiência e uniformidade. Além disso, a função `rand/1` utiliza um algoritmo de fluxo (stream) para garantir que os resultados sejam verdadeiramente aleatórios.

Outra função útil é `Enum.shuffle/1`, que embaralha uma lista de elementos utilizando a função `rand/1`. Isso pode ser útil para sorteios de listas de nomes, por exemplo.

```Elixir
lista = ["João", "Maria", "José", "Ana"]
Enum.shuffle(lista)
```

O resultado será uma nova lista com os elementos embaralhados aleatoriamente.

## Veja também

- [Documentação oficial de Elixir sobre geração de números aleatórios](https://hexdocs.pm/elixir/Kernel.html#rand/1)
- [Artigo sobre Mersenne Twister em Elixir](https://blog.appsignal.com/2018/05/15/elixir-algorithms-mersenne-twister.html)
- [Funções de geração de números aleatórios em Elixir](https://elixir-lang.org/getting-started/random-numbers.html)