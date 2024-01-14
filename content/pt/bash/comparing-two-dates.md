---
title:    "Bash: Comparando duas datas"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas?

Comparar datas é uma tarefa comum na programação, especialmente ao lidar com informações temporais, como datas de vencimento ou prazos de entrega. Saber como comparar duas datas pode ser útil para automatizar processos, fazer cálculos de tempo e muito mais.

## Como Comparar Duas Datas

A linguagem de programação Bash oferece diversas maneiras de comparar datas de forma simples e eficiente. Aqui estão alguns exemplos de como você pode realizar essa tarefa:

### Comparando datas no formato YYYY-MM-DD

Para comparar duas datas no formato padrão YYYY-MM-DD, você pode usar o comando `date` combinado com o operador de comparação `-ge` (greater than or equal to) ou `-le` (less than or equal to). Veja um exemplo:

```Bash
# Define duas datas no formato YYYY-MM-DD
date1="2021-07-15"
date2="2021-07-20"

# Compara as datas usando o comando 'date'
if [[ "$date1" -ge "$date2" ]]; then
  echo "A primeira data é maior ou igual à segunda"
else
  echo "A primeira data é menor ou igual à segunda"
fi
```

A saída será `"A primeira data é menor ou igual à segunda"`, pois 15 de julho (date1) é menor que 20 de julho (date2).

### Comparando datas no formato timestamp

Outra forma de comparar datas é usando o formato timestamp, que representa a quantidade de segundos desde 1 de janeiro de 1970. Para isso, você pode usar os comandos `date +%s` e `bc` para converter as datas em timestamp e então compará-las. Veja um exemplo:

```Bash
# Define duas datas no formato timestamp
date1=$(date -d '2021/07/15' +%s)
date2=$(date -d '2021/07/20' +%s)

# Compara as datas usando o comando 'bc'
if [ "$date1" -ge "$date2" ]; then
  echo "A primeira data é maior ou igual à segunda"
else
  echo "A primeira data é menor ou igual à segunda"
fi
```

A saída será `"A primeira data é menor ou igual à segunda"`, pois 15 de julho (date1) é menor que 20 de julho (date2).

## Mergulhando Mais Fundo

Além das formas mencionadas acima, existem outras maneiras de comparar datas no Bash, como usando a biblioteca `dateutils` ou criando suas próprias funções. É importante também considerar mais informações ao comparar datas, como o horário específico ou até mesmo fuso horário.

## Veja Também

- [Documentação do comando `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Documentação do comando `bc`](https://www.gnu.org/software/bc/manual/html_mono/bc.html)
- [Documentação da biblioteca `dateutils`](https://www.fresse.org/dateutils/#dateutils)