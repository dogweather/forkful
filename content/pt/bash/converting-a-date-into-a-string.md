---
title:                "Convertendo uma data em uma sequência de caracteres"
html_title:           "Bash: Convertendo uma data em uma sequência de caracteres"
simple_title:         "Convertendo uma data em uma sequência de caracteres"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que
Então, você está aprendendo Bash e quer saber como converter uma data em uma string? Yay! A conversão de data em string é uma habilidade importante para trabalhar e manipular datas em arquivos de script em Bash. Então, vamos mergulhar nesse assunto e aprender como fazer isso!

## Como fazer
Para converter uma data em uma string em Bash, você pode usar o comando `date` com algumas opções específicas. Por exemplo, para converter a data atual em uma string, você pode usar o seguinte código:

```Bash
date '+%Y-%m-%d'
```

Este comando irá imprimir a data atual no formato *ano-mês-dia*. Você também pode adicionar texto ao seu string, usando aspas duplas e o comando `echo`, como mostrado abaixo:

```Bash
echo "A data de hoje é: $(date '+%Y-%m-%d')"
```

**Output:** A data de hoje é: 2021-02-15

Outra opção útil é `date '+%F'`, que irá imprimir a data atual no formato *ano-mês-dia* com barras, como "2021/02/15".

## Mergulho profundo
O comando `date` é muito poderoso e versátil. Você pode usá-lo para converter datas em diferentes formatos, incluindo ano, mês, dia, hora, minuto e segundo. Por exemplo, para imprimir a data atual com hora e minuto, você pode usar o seguinte comando:

```Bash
date '+%Y/%m/%d %H:%M'
```

**Output:** 2021/02/15 12:30

Além disso, você também pode usar o comando `date` para converter datas em outros fusos horários, especificando a opção `-u` para UTC ou `-I` para a data ISO 8601. Há muitas opções disponíveis para personalizar o formato da data e hora, então certifique-se de verificar a página de manual do `date` para mais informações.

## Veja também
- [Página de manual do comando `date`](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Tutorial sobre manipulação de datas em Bash](https://www.tecmint.com/format-date-and-time-using-linux-command-line/)