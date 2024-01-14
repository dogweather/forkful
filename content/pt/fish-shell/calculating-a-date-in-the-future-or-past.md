---
title:    "Fish Shell: Calculando uma data no futuro ou passado"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou passado pode ser útil em muitas situações, como agendar atividades ou tarefas, ou para fazer previsões de eventos futuros.

## Como fazer

Para calcular uma data no futuro ou passado usando o Fish Shell, podemos usar o comando `date`. Veja um exemplo abaixo:

```
Fish Shell:
date -d 'tomorrow'
```

A saída seria algo como:

```
seg Mar 22 00:00:00 BRT 2021
```

Vamos entender esse comando passo a passo:

1. A palavra-chave `date` indica que queremos usar o comando para calcular uma data.
2. O parâmetro `-d` diz ao comando que queremos especificar uma data específica.
3. Entre as aspas, podemos inserir qualquer data que desejarmos. Neste exemplo, usamos a palavra "tomorrow", que significa "amanhã".

Você também pode usar outras palavras, como "next week" para calcular uma data no futuro. E se você quiser calcular uma data no passado, basta usar "last week" ou "yesterday".

## Aprofundando

Além de usar palavras para calcular datas, podemos usar números para especificar uma data exata. Por exemplo:

```
Fish Shell:
date -d '3 days'
```

A saída será a data de três dias a partir de hoje. A mesma lógica pode ser aplicada para semanas, meses ou anos.

Outra opção é especificar uma data específica usando o formato "mês/dia/ano". Por exemplo:

```
Fish Shell:
date -d '04/20/2021'
```

A saída será a data especificada: "terça-feixa Abr 20 00:00:00 BRT 2021".

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Calculando datas futuras e passadas com date no Linux](https://www.cyberciti.biz/faq/linux-unix-tomorrow-and-yesterday-days-date-command/)