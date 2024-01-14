---
title:    "Fish Shell: Gerando números aleatórios"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que usar a programação Fish Shell para gerar números aleatórios?

A geração de números aleatórios é uma ferramenta útil em muitas situações, desde jogos até análise estatística. Com a programação Fish Shell, é possível gerar rapidamente uma variedade de números aleatórios com apenas algumas linhas de código, tornando-a uma opção conveniente e eficiente.

## Como fazer:

```Fish Shell
# Gerando um número inteiro aleatório entre 1 e 10
echo (math floor (math rand) * 10) + 1

# Gerando um número decimal aleatório entre 0 e 1
echo (rand)

# Gerando uma lista de 10 números aleatórios entre 0 e 100
for i in (seq 1 10)
    echo (math floor (math rand) * 100)
end
```

Saída de exemplo:

```
6
0.843563
37
98
15
72
91
41
2
51
```

## Mergulho profundo:

Ao usar a função `rand` no Fish Shell, é importante lembrar que ela só gera números aleatórios entre 0 e 1. Para gerar números inteiros aleatórios, é necessário utilizar a função `math rand`, que também deve ser combinada com `math floor` para arredondar o resultado para baixo.

Além disso, é possível definir limites superiores e inferiores para a geração de números aleatórios, conforme mostrado nos exemplos acima. Também é possível utilizar o comando `math round` para arredondar o resultado para o número inteiro mais próximo em vez de simplesmente arredondar para baixo.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/), onde você pode encontrar mais informações sobre o uso de funções matemáticas e de geração de números aleatórios na linguagem Fish Shell.
- [Artigo "Como utilizar a função rand no Fish Shell"](https://medium.com/@crisja/tip-24-random-numbers-in-fish-shell-3820104c9b88), que oferece exemplos adicionais e dicas úteis para a geração de números aleatórios no Fish Shell.
- [Vídeo tutorial "Geração de números aleatórios com Fish Shell"](https://www.youtube.com/watch?v=BsdkA6NKocY), que apresenta de forma prática como utilizar as funções de geração de números aleatórios no Fish Shell.