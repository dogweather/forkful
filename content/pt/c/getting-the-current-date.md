---
title:                "Obtendo a data atual."
html_title:           "C: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que
Algumas vezes, quando estamos desenvolvendo um programa em C, pode ser necessário obter a data atual. Isso pode ser útil para registrar quando um evento ocorreu ou para tornar o programa mais dinâmico, ajustando seu comportamento com base na data atual. Neste artigo, vamos mergulhar na obtenção da data atual em C e mostrar como isso pode ser feito de forma simples e eficiente.

## Como fazer
Em C, a biblioteca "time.h" contém funções para trabalhar com tempo e datas. A função `time()` retorna o número de segundos desde 1º de janeiro de 1970, também conhecido como "epoch". Podemos usar esse valor para calcular a data atual.

```
#include <stdio.h>
#include <time.h>

int main(void) {
    time_t current_time = time(NULL);
    printf("A data e hora atual são: %s\n", ctime(&current_time));
    return 0;
}
```

A saída desse código será algo como:

`A data e hora atual são: Sun Jun 27 15:22:09 2021`

Podemos ver que a função `ctime()` formata o valor retornado pela função `time()` para um formato legível de data e hora.

Também podemos usar a função `localtime()` para obter uma estrutura de data e hora local separada em dia, mês, ano, hora, minuto, segundo, etc.

```
#include <stdio.h>
#include <time.h>

int main(void) {
    time_t current_time = time(NULL);
    struct tm* local_time = localtime(&current_time);
    printf("O ano atual é: %d\n", local_time->tm_year + 1900);
    printf("O mês atual é: %d\n", local_time->tm_mon + 1);
    printf("O dia atual é: %d\n", local_time->tm_mday);
    printf("A hora atual é: %d\n", local_time->tm_hour);
    printf("O minuto atual é: %d\n", local_time->tm_min);
    printf("O segundo atual é: %d\n", local_time->tm_sec);
    return 0;
}
```

A saída desse código será:

```
O ano atual é: 2021
O mês atual é: 6
O dia atual é: 27
A hora atual é: 15
O minuto atual é: 39
O segundo atual é: 5
```

Outra forma de obter a data atual em um formato específico é usar a função `strftime()`. Essa função permite que você especifique o formato da data e hora desejado, usando especificadores de formato. Por exemplo, podemos exibir a data no formato "dia/mês/ano".

```
#include <stdio.h>
#include <time.h>

int main(void) {
    time_t current_time = time(NULL);
    char str_date[11];
    struct tm* local_time = localtime(&current_time);
    strftime(str_date, sizeof(str_date), "%d/%m/%Y", local_time);
    printf("A data atual é: %s\n", str_date);
    return 0;
}
```

A saída desse código será:

`A data atual é: 27/06/2021`

## Deep Dive
Agora que já sabemos como obter a data atual em C, vamos entender melhor como essa obtenção funciona. Como mencionado anteriormente, a função `time()` retorna o número de segundos desde 1º de janeiro de 1970. Esse valor é conhecido como "timestamp" e é a forma padrão de armazenar datas e horas em sistemas Unix.

A biblioteca "time.h" também possui outras funções para manipular datas e horas, como `mktime()` para converter uma estrutura de data e hora em um timestamp e `gmtime()` para obter uma estrutura de data e hora UTC (Tempo Universal Coordenado).

É importante lembrar que a função `time()` retorna um valor do tipo `time_t`, que pode variar em tamanho dependendo da plataforma em que o programa está sendo executado. É aconselhável usar funções de conversão adequadas para evitar possíveis problemas de compatibilidade.

## See Also
* [Documentação da biblioteca "time.h" em cplusplus.com](http://www.cplusplus.com/reference/ctime/)
* [Tutorial da Codeacademy sobre datas e horas em C](https://www.codecademy.com/learn/learn-c/modules/learn-c-dates-and-times)