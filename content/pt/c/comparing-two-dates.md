---
title:    "C: Comparando duas datas"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que comparar duas datas em C?

Comparar duas datas em um programa C é uma tarefa importante para muitas aplicações que envolvem controle de tempo e datas. Seja para verificar se uma data é anterior ou posterior a outra, calcular a diferença de dias entre duas datas ou validar a entrada de datas do usuário, a comparação de datas é uma habilidade essencial para qualquer programador em C. Neste artigo, vamos explorar como realizar essa tarefa de forma eficiente e sem complicações.

## Como comparar datas em C

Para comparar duas datas em C, podemos utilizar a biblioteca padrão `time.h` que fornece funções úteis para trabalhar com datas e tempo. Vamos dar uma olhada em um código simples que compara duas datas e exibe o resultado:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // criando estruturas tm para representar duas datas
    struct tm data1 = { .tm_year = 2021, .tm_mon=05, .tm_mday=12 };
    struct tm data2 = { .tm_year = 2020, .tm_mon=07, .tm_mday=21 };

    // convertendo as estruturas tm em valores time_t
    time_t t1 = mktime(&data1);
    time_t t2 = mktime(&data2);

    // comparando as datas usando a funcao difftime()
    double diferenca = difftime(t1, t2);

    // exibindo o resultado da comparacao
    if (diferenca == 0) {
        printf("Ambas as datas sao iguais");
    } else if (diferenca > 0) {
        printf("A data1 e posterior a data2");
    } else {
        printf("A data2 e posterior a data1");
    }

    return 0;
}
```

**Saída:**

```
A data1 e posterior a data2
```

Neste exemplo, criamos duas datas utilizando a estrutura `tm` e em seguida, utilizamos a função `mktime()` para convertê-las em valores `time_t`, que representam a quantidade de segundos desde a meia-noite de 1º de janeiro de 1970. Em seguida, utilizamos a função `difftime()` para calcular a diferença entre as datas e exibimos o resultado da comparação.

É importante lembrar que as datas devem ser fornecidas no formato correto (ano, mês e dia) para que a função `mktime()` faça a conversão corretamente. Além disso, a função `difftime()` retorna um valor em segundos, então é necessário fazer a conversão para dias, horas, minutos, etc. caso seja necessário.

## Mergulho profundo

Se quisermos nos aprofundar mais na comparação de datas em C, é importante entender como a biblioteca `time.h` lida com datas e tempo. A função `mktime()` é responsável por fazer a conversão de uma estrutura `tm` em um valor `time_t`, que representa a data em segundos. Já a função `difftime()`, como vimos anteriormente, é utilizada para calcular a diferença entre dois valores `time_t`.

Outra função útil para comparar datas é a função `gmtime()`, que permite converter um valor `time_t` em uma estrutura `tm` e extrair informações específicas, como ano, mês, dia, hora, etc. Para saber mais sobre essas funções e outras relacionadas a datas e tempo em C, é recomendado consultar a documentação oficial da biblioteca `time.h`.

## Veja também

- [Documentação oficial da biblioteca `time.h`](https://en.cppreference.com/w/c/chrono)
- [Tutorial sobre funções de data e tempo em C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Exemplos de código para comparar datas em C](https://www.techonthenet.com/c_language/standard_library_functions/time_h/difftime.php)