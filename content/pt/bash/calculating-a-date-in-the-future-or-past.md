---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Bash: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que e por que?

Calcular uma data no futuro ou no passado é um processo comum na programação que envolve o cálculo de uma data com base em uma data inicial e uma quantidade específica de tempo. Os programadores podem precisar calcular datas futuras ou passadas para tarefas como agendar eventos, rastrear prazos ou gerar relatórios.

## Como fazer:

Existem várias maneiras de calcular datas futuras ou passadas usando Bash. Aqui estão dois exemplos comuns que utilizam os comandos 'date' e 'strtotime':

```
# Calculando 30 dias no futuro a partir de hoje
future_date=$(date -d "+30 days" +%Y-%m-%d)
echo "Data Futura: $future_date"

# Calculando 1 ano no passado a partir de uma data específica
past_date=$(date -d "2020-01-01 - 1 year" +%Y-%m-%d)
echo "Data Passada: $past_date"
```

A saída desses comandos seria:

```
Data Futura: 2021-02-18
Data Passada: 2019-01-01
```

## Mergulho profundo:

A capacidade de calcular datas no futuro ou no passado é uma funcionalidade importante para qualquer linguagem de programação, incluindo Bash. Historicamente, esse processo era feito manualmente com matemática básica, mas agora, com a ajuda de comandos como 'date' e 'strtotime', é possível obter resultados mais precisos de forma mais fácil e eficiente.

Além disso, existem alternativas para calcular datas no Bash, como a biblioteca 'tdate' que fornece funções mais avançadas para cálculos de datas. No entanto, para tarefas simples, os comandos 'date' e 'strtotime' geralmente são suficientes.

É importante notar também que o formato de data usado nos exemplos pode variar dependendo do sistema operacional e do local configurados. Portanto, é sempre bom verificar qual o formato correto a ser utilizado antes de realizar cálculos de datas com o Bash.

## Veja também:

- [Página do Manual do Bash sobre o comando 'date'](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-date)
- [Página do Manual do Bash sobre o comando 'strtotime'](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-strtotime)
- [Biblioteca tdate](https://github.com/eckz/tdate)