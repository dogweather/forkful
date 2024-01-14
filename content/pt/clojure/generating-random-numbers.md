---
title:    "Clojure: Gerando números aleatórios"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante

Gerar números aleatórios é importante em muitos casos, desde jogos até análises estatísticas. Com a função de geração de números aleatórios do Clojure, você pode facilmente adicionar essa funcionalidade aos seus programas.

## Como fazer

Para gerar um único número aleatório, utilize a função `rand`. Por exemplo:

```Clojure
(rand)
```

Isso irá gerar um número decimal aleatório entre 0 e 1. Para gerar um número inteiro, você pode multiplicar o resultado por um número específico, como 10:

```Clojure
(* (rand) 10)
```

Você também pode definir um intervalo específico, utilizando a função `rand-int`. Por exemplo, para gerar um número inteiro entre 1 e 100:

```Clojure
(rand-int 100)
```

## Mergulhando mais fundo

A geração de números aleatórios no Clojure é baseada no gerador de números aleatórios do sistema Java. Isso significa que a semente (seed) do gerador é compartilhada com outras partes do sistema Java, e pode ser definida manualmente usando a função `setSeed`. Por exemplo, para gerar sempre os mesmos números aleatórios, você pode definir uma semente constante:

```Clojure
(setSeed 1234)
```

Com isso, sempre que você chamar a função `rand` ou `rand-int`, o resultado será o mesmo. Isso pode ser útil para testes e depuração.

## Veja também

- [Documentação oficial do Clojure sobre geração de números aleatórios](https://clojuredocs.org/clojure.core/rnd)
- [Tutorial sobre geração de números aleatórios no Clojure](https://clojurescriptmadeeasy.com/blog/random-numbers-in-clojure/)
- [Artigo sobre geração de números aleatórios em linguagens de programação](https://en.wikipedia.org/wiki/Random_number_generation_in_programming_languages)