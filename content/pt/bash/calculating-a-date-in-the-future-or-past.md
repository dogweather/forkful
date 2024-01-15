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

## Por que

Calcular uma data no futuro ou no passado pode ser útil em diversas situações, desde planejar uma viagem até realizar cálculos financeiros. O Bash tem funções e comandos integrados que facilitam esse tipo de cálculo, tornando a tarefa mais simples e rápida.

## Como Fazer

Para calcular uma data no futuro ou no passado no Bash, você pode usar o comando `date` combinado com o parâmetro `-d` para especificar uma data e o parâmetro `-v` para adicionar ou subtrair dias, semanas, meses ou anos. Veja alguns exemplos:

```
# Calcular a data de hoje mais 2 dias
date -d "+2 days"

# Calcular a data de hoje há 2 meses
date -d "-2 months"

# Calcular a data de 20/10/2021 mais 1 semana
date -d "20/10/2021 +1 week"

# Calcular a data de 01/01/2022 há 3 anos
date -d "01/01/2022 -3 years"
```

O comando `date` possui uma sintaxe flexível que permite especificar a data de diversas maneiras, como por exemplo:

- MM/DD/YY: mês/dia/ano
- DD/MM/YY: dia/mês/ano
- YYYY/MM/DD: ano/mês/dia

Além disso, você também pode usar abreviações para representar as unidades de tempo, como `d` para dias, `w` para semanas, `m` para meses e `y` para anos.

## Deep Dive

Além do comando `date`, o Bash também possui funções internas que podem ser usadas para calcular datas. Um exemplo é a função `dateadd`, que permite adicionar ou subtrair dias, semanas, meses ou anos de uma data específica.

Por exemplo, para calcular a data de hoje mais 2 semanas utilizando a função `dateadd`, você pode usar o seguinte comando:

```
# Calcular a data de hoje mais 2 semanas
dateadd now +2 weeks
```

Além disso, o Bash também possui a função `dateval`, que permite validar uma data e verificar se ela é válida ou não. Isso pode ser útil para evitar erros em cálculos futuros ou passados.

Para utilizar a função `dateval`, você pode usar o seguinte comando:

```
# Validar a data 30/02/2021
dateval 30/02/2021
```

Se a data for válida, o comando não retornará nenhuma mensagem. Porém, se a data for inválida, ele irá exibir uma mensagem de erro.

## Veja Também

- [Documentação do Comando `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Funções Internas do Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html)