---
title:                "Analisando uma data de uma string"
html_title:           "Bash: Analisando uma data de uma string"
simple_title:         "Analisando uma data de uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
O parsing de uma data a partir de uma string é o processo de extrair a data contida em uma string de formato específico, transformando-a em um formato padrão. Programadores fazem isso para que possam manipular, comparar e exibir datas de forma mais eficiente em seus programas.

## Como Fazer:
Existem várias maneiras de fazer o parsing de uma data em Bash, mas a mais comum é usando o comando `date`. Por exemplo, para extrair a data de uma string no formato "dd/mm/yyyy", podemos usar o seguinte comando:

```Bash
data="15/05/2020"
data_formatada=$(date -d "$data" +%Y-%m-%d)
echo "$data_formatada" # resultado: 2020-05-15
```

Também é possível fazer o parsing de datas usando expressões regulares e funções do Bash, porém isso requer um nível maior de conhecimento técnico.

## Mergulho Profundo:
Fazer o parsing de datas é uma tarefa comum em programação desde os primórdios da computação. Antigamente, não existiam linguagens de programação avançadas como o Bash e todas as tarefas eram feitas usando comandos de terminal. No entanto, com o avanço da tecnologia, surgiram alternativas mais robustas para fazer o parsing de datas, como a linguagem Python, que possui bibliotecas específicas para esse fim.

Para fazer o parsing de datas em Bash, também é possível usar a função `read` para ler valores de uma string e manipulá-los posteriormente. Além disso, é importante ter em mente que o formato da data pode variar em diferentes sistemas operacionais e países, portanto, é necessário ter cuidado ao manipular datas em um programa.

## Veja Também:
- [Documentação oficial do comando `date` em Bash](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Exemplos de uso da função `read` em Bash](https://www.howtogeek.com/654973/how-to-read-data-from-a-user-on-the-linux-command-line/)
- [Biblioteca Python para parsing de datas](https://docs.python.org/3/library/datetime.html)