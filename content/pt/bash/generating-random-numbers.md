---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por quê?

Gerar números aleatórios é criar uma sequência de números que não possui padrão previsível. Programadores fazem isso para criar dados de teste, simular cenários em jogos, e para decisões de software que precisam ser imprevisíveis.

## Como fazer:

Em Bash, você pode gerar números aleatórios usando a variável especial `$RANDOM`. Aqui temos um exemplo:

```Bash
echo $RANDOM
```

A execução do código acima produzirá um número entre 0 e 32767. Para obter um número em um intervalo específico, use a operação módulo `%`.

```Bash
echo $((RANDOM % 100))
```

## Mergulho Profundo

A variável `$RANDOM` possui um longo histórico. Originou-se no Unix, e tem sido incorporada na maioria das línguas de script baseadas em Shell, incluindo Bash.

Alternativamente, se precisar de um número aleatório mais significativo, como um UUID, você pode utilizar `uuidgen`:

```Bash
uuidgen
```

Internamente, `$RANDOM` usa um gerador de números pseudo-aleatórios. Isso significa que, enquanto parece aleatório, se soubermos a "seed" (semente) usada podemos prever os números gerados. Entretanto, para a maioria dos usos, essa previsibilidade não é um problema.

## Veja também

Por fim, aqui estão algumas outras fontes úteis sobre geração de números aleatórios em programas:

1. Manual bash (man bash): O manual oficial do bash. Procure por `$RANDOM`.
2. Documentação "uuidgen" (man uuidgen): Mais detalhes sobre como gerar um UUID no Bash.