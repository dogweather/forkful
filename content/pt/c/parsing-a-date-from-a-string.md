---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

---

## O Que & Porquê?

Analisar uma data a partir de uma string é extrair valores específicos como dia, mês e ano de uma cadeia de caracteres. Programadores fazem isso para manipular, formatar ou comparar datas de maneira personalizada.

## Como Fazer

Vamos usar a função `strptime` que faz parte da biblioteca de tempo padrão em C. Neste exemplo, vamos transformar uma string em uma estrutura de data e tempo:

```C
#include <time.h>
#include <stdio.h>

int main() {
    struct tm temp;
    const char *date_string = "27/09/2020";
    strptime(date_string, "%d/%m/%Y", &temp);

    printf("Ano: %d \n", temp.tm_year + 1900);
    printf("Mês: %d \n", temp.tm_mon + 1);
    printf("Dia: %d \n", temp.tm_mday);
    return 0;
}
```

A saída será:

```
Ano: 2020 
Mês: 9 
Dia: 27 
```

## Aprofundando

### Contexto Histórico 
A função `strptime` está em uso desde os primeiros sistemas Unix e foi posteriormente incluída na biblioteca C padronizada. 

### Alternativas
Outras opções incluem escrever sua própria função de análise ou usar bibliotecas de terceiros como o `GNU Library` ou `Boost DateTime`.

### Detalhes de implementação
O analisador de datas da string convertida em uma estrutura `tm` pela função `strptime` pode variar dependendo do ambiente. Certifique-se de fornecer o formato de data correto.

## Ver Também

* Documentação da função `strptime`: [cplusplus.com](http://www.cplusplus.com/reference/ctime/strptime/)
* Tutorial completo sobre a biblioteca de tempo em C: [tutorialspoint.com](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
* Outras funções de manipulação de tempo e data: [geekhideout.com](http://www.geekhideout.com/urlcode.shtml)

---