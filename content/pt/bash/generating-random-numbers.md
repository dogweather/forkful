---
title:                "Gerando números aleatórios"
html_title:           "Bash: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que é e Porquê?

Gerar números aleatórios é um processo em que um programa de computador produz números sem seguir um padrão específico, tornando imprevisível o próximo número gerado. Programadores fazem isso para criar jogos, simulações e algoritmos que precisam de aleatoriedade para funcionar corretamente.

## Como fazer:

Existem várias maneiras de gerar números aleatórios em Bash, mas aqui estão algumas opções simples usando o comando `shuf`:

```Bash
# Gerando um número aleatório entre 0 e 100
echo $(( $(shuf -i 0-100 -n 1) ))

# Gerando um número aleatório entre 1 e 10
echo $(( $(shuf -i 1-10 -n 1) ))

# Gerando uma letra aleatória entre A e Z
echo $( shuf -n 1 -e {A..Z} )
```

A saída será um número ou uma letra aleatória, dependendo do comando usado.

## Profundidade:

A geração de números aleatórios tem sido uma parte importante da computação desde o início. Um dos algoritmos mais comuns é o Método Congruente Linear, criado em 1949. Outras alternativas em Bash incluem o comando `fold`, que lê dados de entrada e mistura suas linhas de forma aleatória, e o `random`, que gera números aleatórios de 0 a 32767.

## Veja também:

- [Documentação oficial do GNU Bash](https://www.gnu.org/software/bash/)
- [Tutorial de Bash na Udemy](https://www.udemy.com/course/bash-scripting/)
- [Job de programador na Caelum](https://www.caelum.com.br/aprenda/programacao/trabalhe-na-caelum/) (precisa saber Bash!)