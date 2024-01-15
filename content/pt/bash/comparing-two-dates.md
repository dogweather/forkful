---
title:                "Comparando duas datas"
html_title:           "Bash: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Datas é Importante?

Comparar datas é uma tarefa essencial em programação, especialmente quando se trata de lidar com dados de diferentes fontes ou em diferentes formatos. Ao comparar duas datas, podemos determinar a ordem, a diferença ou a igualdade entre elas, o que pode ser muito útil na manipulação de dados.

## Como Comparar Datas em Bash

Para comparar duas datas em Bash, podemos usar a ferramenta `date` e a sintaxe de expansão de comando `$()` para converter as datas em um formato que possa ser facilmente comparado. Aqui está um exemplo simples:

```Bash
data1=$(date -d "2020-01-01" +%s) # converte data1 para o formato Unix timestamp
data2=$(date -d "2020-02-01" +%s) # converte data2 para o formato Unix timestamp

if [ $data1 -lt $data2 ]; then
  echo "A data1 é anterior à data2."
fi

# saída: A data1 é anterior à data2.
```

Neste exemplo, usamos a opção `-d` para especificar as datas que queremos converter, seguidas do formato desejado com a opção `%s` para obter o formato Unix timestamp. Em seguida, comparamos os valores de timestamp das datas usando a sintaxe `if [ $data1 -lt $data2 ]` para verificar se a data1 é anterior à data2, e caso seja, imprimimos a mensagem correspondente.

## Profundidade na Comparação de Datas

Existem vários formatos em que as datas podem ser comparadas, como o formato de data completa `yyyy-mm-dd`, o formato de data abreviada `dd/mm/yyyy` ou o formato de timestamp. É importante ter isso em mente ao comparar datas em Bash, pois o resultado pode ser diferente dependendo do formato usado. Além disso, também é possível usar as opções `-d` e `-f` da ferramenta `date` para especificar diferentes formatos de data e informações de fuso horário.

## Veja Também

- [Documentação do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Documentação do `date` command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)