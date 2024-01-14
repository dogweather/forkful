---
title:                "C: Calculando uma data no futuro ou no passado"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado é uma função muito útil e comum em programação. Pode ser necessário para agendar eventos ou tarefas, ou para calcular a idade de uma pessoa em um determinado dia. Com habilidades básicas em linguagem de programação C, é possível fazer esse cálculo facilmente. 

## Como fazer

Para calcular uma data no futuro ou no passado, primeiro precisamos entender como as datas são representadas em C. A linguagem usa uma estrutura chamada `struct tm` que contém informações sobre o ano, mês, dia, hora, minuto, segundo e fuso horário.

Para usar essa estrutura, precisamos incluir a biblioteca `<time.h>` no início do código. Em seguida, declaramos uma variável do tipo `struct tm` e atribuímos valores a ela. Por exemplo, para representar o dia 19 de agosto de 2021, podemos fazer o seguinte:

```C
struct tm data;
data.tm_year = 121; // Ano - 1900 = 2021
data.tm_mon = 7; // Mês - 1 = Agosto
data.tm_mday = 19; // Dia
```

A partir daqui, podemos usar as funções `mktime()` e `localtime()` para converter a estrutura `struct tm` em um formato de tempo que possamos manipular. Em seguida, podemos usar a função `strftime()` para formatar essa data em uma string, que pode ser impressa na tela ou usada em outros cálculos. Aqui está um exemplo de código completo para calcular a data 7 dias no futuro:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm data;
    data.tm_year = 121;
    data.tm_mon = 7;
    data.tm_mday = 19;

    // Converter a estrutura para formato de tempo
    time_t time = mktime(&data);
    // Adicionar 7 dias ao formato de tempo
    time += 7 * 24 * 60 * 60;
    // Converter novamente para struct tm
    data = *localtime(&time);

    // Formatando a data em uma string
    char data_string[11];
    strftime(data_string, 11, "%d/%m/%Y", &data);

    // Imprimir na tela
    printf("Daqui a 7 dias será: %s", data_string);

    return 0;
}
```

A saída deste programa será:

```
Daqui a 7 dias será: 26/08/2021
```

Podemos usar esse mesmo conceito para calcular datas no passado, basta subtrair o número de segundos correspondentes ao tempo desejado em vez de adicionar. Existem muitas outras funções e técnicas que podem ser usadas para trabalhar com datas em C, mas essas são as mais básicas e úteis.

## Aprofundando

No exemplo acima, usamos a função `strftime()` para formatar a data em uma string. Essa função é muito útil e possui vários argumentos que podem ser usados para formatar a data de diferentes maneiras. Para saber mais sobre eles e outras funções relacionadas a datas, você pode conferir a documentação [aqui](https://www.tutorialspoint.com/c_standard_library/time_h.htm).

No entanto, é importante notar que essa forma de representar datas em C só é válida para datas a partir de 1970, pois é quando a contagem de tempo começa. Para datas anteriores a isso, é necessário usar outras estruturas e algoritmos mais complexos.

## Veja também

- [Como trabalhar com datas em C](https://pt.wikipedia.org/wiki/Time.h)
- [Documentação da biblioteca <time.h> em C](https://www.ibm.com/docs/en/i/7.4?topic=functions-time-time-date-and-time-variable-handling-functions)
- [Tutorial de datas com <time.h> em C](https://www.gnu.org/software/libc/manual/html_node/Time-Function-Example.html)