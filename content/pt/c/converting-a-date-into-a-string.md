---
title:    "C: Convertendo uma data em uma string"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que

Ao longo do desenvolvimento de um programa em C, pode ser necessário converter uma data em formato string. Isso pode ser útil para mostrar informações ao usuário em um formato legível ou para armazenar dados em um formato mais fácil de manipular. Neste artigo, vamos explorar a conversão de data para string em C e como isso pode ser útil em seus projetos.

## Como fazer

Para converter uma data em string em C, vamos utilizar a função `strftime()` da biblioteca `time.h`. Esta função permite converter uma estrutura de dados `struct tm` em uma string no formato desejado. Veja um exemplo de código:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Pegando a data e hora atual
  time_t current_time;
  time(&current_time);

  // Convertendo para struct tm
  struct tm *time_info = localtime(&current_time);

  // Convertendo para string
  char date_string[50];
  strftime(date_string, sizeof(date_string), "%d/%m/%Y %H:%M:%S", time_info);

  // Imprimindo a data convertida
  printf("A data e hora atual é: %s\n", date_string);
  
  return 0;
}
```

Este código irá imprimir a data e hora atual em formato de string. No caso, a saída será: "A data e hora atual é: 15/04/2021 13:45:32". Vamos analisar o trecho de código que faz a conversão:

```C
strftime(date_string, sizeof(date_string), "%d/%m/%Y %H:%M:%S", time_info);
```

O primeiro parâmetro é o array de caracteres onde a string será armazenada. O segundo é o tamanho da string, que deve ser grande o suficiente para armazenar a data e o tempo formatados. O terceiro é o formato da string que queremos, neste caso, dia, mês, ano, hora, minuto e segundo. Por fim, o último parâmetro é a estrutura de dados `struct tm` que contém a data e hora que queremos converter.

## Mergulho profundo

É importante entender a estrutura de dados `struct tm` para conseguir utilizar a função `strftime()` corretamente. Ela contém informações sobre a data e hora em vários campos, como dia, mês, ano, hora, minuto e segundo. Além disso, também possui campos para fuso horário, dia da semana, entre outros.

Outras funções da biblioteca `time.h` também podem ser úteis, como `localtime()` para converter um objeto `time_t` em `struct tm`, e `mktime()` para converter uma estrutura `struct tm` em `time_t`.

## Veja também

- [Documentação oficial da biblioteca time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Tutorial de C sobre data e hora](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Exemplos de códigos em C](https://www.programiz.com/c-programming/examples)