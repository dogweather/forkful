---
title:                "Fish Shell: Gerando números aleatórios"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que
Às vezes, em nosso código, precisamos gerar números aleatórios para realizar certas operações. Seja para testar algoritmos, criar jogos ou qualquer outra aplicação que requeira elementos não previsíveis, a geração de números aleatórios é indispensável. Neste artigo, vamos explorar como podemos fazer isso de forma simples e eficiente utilizando o Fish Shell.

## Como fazer
Para gerar números aleatórios no Fish Shell, podemos utilizar a função `math/rand` com o operador `&`, que nos dá um valor aleatório entre 0 e 1. Podemos multiplicá-lo pelo intervalo desejado e depois adicionar um número base para obter os valores desejados.

```
Fish Shell - geração de números aleatórios

#declarando o intervalo e o número base
set interval 10
set base 5

#gerando um número aleatório
echo (math/rand & * $interval + $base)
```

Por exemplo, se quisermos gerar um número aleatório entre 5 e 15, podemos multiplicar o resultado da função `math/rand &` por 10 e adicionar 5.

Outra opção é utilizar a função `echo -n` em conjunto com a função `fmt` para formatar o número gerado em uma unidade desejada, como inteiro ou decimal.

```
Fish Shell - formatação de números aleatórios

#gerando um número aleatório como inteiro
echo -n (math/rand &) | fmt -o "%.0f"

#gerando um número aleatório como decimal
echo -n (math/rand &) | fmt -o "%.2f"
```

## Deep Dive
Por baixo dos panos, a função `math/rand` utiliza o gerador de números aleatórios do sistema operacional. Isso garante que os números gerados sejam verdadeiramente aleatórios e não influenciados por fatores externos. Além disso, podemos utilizar o operador `|` para passar o resultado da função `math/rand &` para outras funções, como `sort` ou `head`, para obter conjuntos de números aleatórios em diferentes formatos.

```
Fish Shell - manipulação de números aleatórios

#gerando uma lista de 10 números aleatórios entre 1 e 100
for i in (seq 10)
	echo (math/rand & * 100 + 1) | head -n 1 | fmt -o "%.0f"
end

#ordenando uma lista de números aleatórios
for i in (seq 10)
	echo (math/rand & * 100 + 1) | sort -n | fmt -o "%.0f"
end
```

## Veja também
Este artigo cobriu apenas uma pequena parcela da funcionalidade do Fish Shell. Para saber mais sobre suas características e funcionalidades, recomendo verificar os links abaixo:

- [Documentação do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Comunidade Fish Shell](https://github.com/fish-shell/fish-shell)
- [Repositório de plugins para Fish Shell](https://github.com/jorgebucaran/fisher)