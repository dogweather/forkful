---
title:                "C: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Você já se perguntou como seria calcular uma data no futuro ou no passado? Talvez seja para planejar uma viagem ou para fazer uma programação de eventos. Independentemente do motivo, calcular uma data específica pode ser extremamente útil em diferentes situações.

## How To
Para calcular uma data em C, podemos utilizar a biblioteca de funções "time.h". Primeiro, precisamos declarar as variáveis necessárias para armazenar a data atual e a data desejada. Em seguida, utilizamos a função "time()" para obter a data atual em segundos. A partir disso, podemos utilizar a função "localtime()" para converter o tempo em segundos para uma estrutura de data que pode ser manipulada.

```
#include <stdio.h>
#include <time.h>

int main(){
    struct tm *atual, *data_final;
    time_t atual_seg, final_seg;

    // Obtém a data atual em segundos
    time(&atual_seg);

    // Converte a data atual para uma estrutura de data
    atual = localtime(&atual_seg);

    // Define a data desejada (ano, mês, dia)
    data_final->tm_year = 2021 - 1900; // 2021 é representado por 1900 em C
    data_final->tm_mon = 11 - 1; // Novembro é representado como 10 em C
    data_final->tm_mday = 25;

    // Converte a data desejada para segundos
    final_seg = mktime(data_final);

    // Calcula a diferença entre as datas em dias
    double days = difftime(final_seg, atual_seg) / (60*60*24);

    // Imprime o resultado
    printf("Faltam %.0f dias para o Natal de 2021!", days);

    return 0;
}
```

Exemplo de saída: Faltam 127 dias para o Natal de 2021!

## Deep Dive
Para calcular uma data no futuro ou no passado, é importante entender como as datas são representadas em C. Elas são armazenadas em segundos a partir de 01/01/1970. Ou seja, o valor 0 representaria essa data específica e valores positivos ou negativos indicam datas antes ou depois disso.

Além disso, é preciso ter cuidado com anos bissextos ao realizar cálculos com datas. E, caso seja necessário, é possível adicionar ou subtrair segundos a uma data utilizando a função "mktime()".

## See Also
- [Documentação oficial da biblioteca "time.h" em C](https://en.cppreference.com/w/c/chrono)
- [Explicação detalhada sobre representação de datas em C](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm)
- [Outras funções úteis para manipulação de datas em C](http://www.cplusplus.com/reference/ctime/)