---
title:                "Analisando uma data de uma string"
html_title:           "C: Analisando uma data de uma string"
simple_title:         "Analisando uma data de uma string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Fazer a análise de uma data em uma string é o processo de extrair informações de uma string que represente uma data e convertê-la em um formato que possa ser manipulado pelo computador. Os programadores realizam esse processo para poderem trabalhar com as datas de forma mais eficiente em seus programas.

## Como Fazer:

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int main() {

    // Exemplo de string de data no formato DD/MM/YYYY
    char data_str[11] = "31/12/2020";

    // Criando uma estrutura tm que irá armazenar as informações da data
    struct tm data;

    // Usando a função strptime() para converter a string em informações de data e armazená-las na estrutura
    strptime(data_str, "%d/%m/%Y", &data);

    // Imprimindo as informações da estrutura com a função printf() 
    printf("Dia: %d\n", data.tm_mday); // Imprime 31 
    printf("Mês: %d\n", data.tm_mon + 1); // Imprime 12 (Janeiro é representado pelo número 0)
    printf("Ano: %d\n", data.tm_year + 1900); // Imprime 2020 (Para obter o ano correto, é necessário somar 1900)

    return 0;
}
```

Saída:

```
Dia: 31
Mês: 12
Ano: 2020
```

## Deep Dive:

Analisar uma data a partir de uma string é um processo comum e importante em programação. Antes da introdução de bibliotecas e funções específicas para lidar com datas e tempo, os programadores precisavam criar algoritmos complexos para realizar essa tarefa. No entanto, com o avanço da tecnologia, foram criadas várias bibliotecas e funções que facilitaram esse processo. Além da função `strptime()`, a linguagem C também oferece outras funções como `strftime()` e `mktime()` para trabalhar com datas.

## Veja Também:

- [Documentação da função `strptime()`](https://www.cplusplus.com/reference/ctime/strptime/)
- [Tutorial sobre manipulação de datas em C](https://www.cyberciti.biz/faq/howto-convert-linux-unix-time-in-seconds-to-string/)
- [Outros formatos de data suportados pela função `strptime()`](https://www.freebsd.org/cgi/man.cgi?f=strptime&manpath=SuSE+Linux/i386+11.2)