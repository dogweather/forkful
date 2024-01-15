---
title:                "Obtendo a data atual"
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que utilizar Bash?

Se você trabalha com programação de sistemas ou tem interesse em automatizar tarefas no seu computador, é provável que já tenha ouvido falar sobre o Bash. Uma das vantagens dessa linguagem de script é a sua capacidade de interagir com o sistema operacional, permitindo a execução de comandos do terminal. E uma das funções mais úteis do Bash é a possibilidade de obter a data e a hora atual.

## Como obter a data atual em Bash?

Obter a data atual é uma tarefa simples em Bash. Basta usar o comando `date`, seguido das opções desejadas. Por padrão, o comando `date` retorna a data e a hora atual no formato dd/MM/yyyy HH:mm:ss, mas é possível personalizar o formato usando a opção `-format`. Veja um exemplo:

```Bash
date "+%d/%m/%Y"
```
Output: 29/08/2021

Outra opção interessante é a possibilidade de exibir a data e a hora em fuso horário diferente do seu sistema. Basta usar a opção `-u` seguida do fuso horário desejado. Veja um exemplo:

```Bash
date -u "+%d/%m/%Y %H:%M:%S" # Fuso horário UTC
```
Output: 29/08/2021 21:31:15

Há diversas outras opções que podem ser usadas com o comando `date` para personalizar a forma como a data e a hora são exibidas. Para conhecer todas elas, você pode conferir a documentação oficial do Bash ou utilizar o comando `man date` no seu terminal.

## Uma olhada mais aprofundada na obtenção da data atual

Quando usamos o comando `date`, na verdade estamos fazendo uma chamada de sistema para obter a data e a hora atual. O Bash, por sua vez, é responsável por interpretar e formatar essa informação de acordo com as opções fornecidas. Isso significa que, se você precisar obter a data e a hora em um formato específico que não está disponível no comando `date`, pode ser necessário implementar uma solução personalizada em Bash para fazer essa formatação.

Outro detalhe interessante é que o comando `date` fica armazenado em um arquivo executável no seu sistema operacional, geralmente na pasta /usr/bin. Isso significa que você pode usá-lo em qualquer linguagem de programação que suporte a chamada de comandos do sistema, não apenas em Bash.

## Veja também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/bash.html#Bash-Features)
- [Guia rápido sobre o comando date](https://www.gbdirect.co.uk/resources/nix/kb/date.html)
- [Artigo sobre como formatar a data em Bash](https://www.howtogeek.com/410442/how-to-display-the-date-time-using-the-linux-command-line/)