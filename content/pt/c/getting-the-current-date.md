---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que e Porquê?

Obter a data atual é uma função útil que informa em tempo real o ano, mês e dia corrente. Programadores usam essa ferramenta para registrar informações sazonais, monitorizar eventos, ou implementar funções relacionadas ao tempo real.

## Como fazer:

Aqui está um exemplo de como obter a data atual em C:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    printf("Agora é: %02d/%02d/%04d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);

    return 0;
}
```

Se executar este código, a saída vai ser algo como:

```
Agora é: 23/04/2022
```

## Deep Dive:

Na história do C, o manejo do tempo sempre foi um processo complexo devido aos fatores como fusos horários. No exemplo acima, usamos a biblioteca `time.h` que possui funções inerentes para lidar com o tempo e a data.

Em alternativa, pode-se utilizar funções específicas de sistemas Unix ou Windows para obter a data, mas o método mostrado acima é o mais completo e portável.

A função `time(NULL)` retorna o tempo atual do sistema em segundos desde 1º de janeiro de 1970 - um evento conhecido como Epoch UNIX. `localtime()` converte esse valor para uma estrutura legível.

## Ver também:

- Documentação oficial da biblioteca time.h: www.cplusplus.com/reference/ctime/
- Artigos detalhados sobre manejo do tempo em C: https://www.geeksforgeeks.org/time-function-in-c/