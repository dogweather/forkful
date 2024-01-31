---
title:                "Convertendo uma data em uma string"
date:                  2024-01-20T17:35:57.231634-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma data em uma string"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data em uma string permite manipular e exibir datas como texto. Programadores fazem isso para facilitar a interação com usuários, salvar em arquivos ou trabalhar com APIs que requerem datas em formatos específicos.

## Como Fazer:
O padrão C fornece a biblioteca `time.h` para lidar com datas e horas. Abaixo um exemplo de código que mostra como converter uma data em uma string:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t current_time;
    struct tm *time_info;
    char date_str[80];

    time(&current_time); // Pega o tempo atual
    time_info = localtime(&current_time); // Converte para hora local

    // Formata a data e hora em uma string
    strftime(date_str, sizeof(date_str), "%d/%m/%Y %H:%M:%S", time_info);
  
    printf("Data e hora atuais: %s\n", date_str);

    return 0;
}
```

Saída de exemplo:

```
Data e hora atuais: 31/03/2023 16:45:12
```

## Aprofundando
Historicamente, lidar com datas sempre foi um desafio na programação por causa de fusos horários, formatos e a complexidade inerente ao tempo (pense em anos bissextos, por exemplo). No C, isso se resolve utilizando `time.h`. Mas há alternativas como a biblioteca `chrono` em C++ ou bibliotecas de terceiros como a `date.h`.

Detalhes de implementação envolvem entender a função `strftime()`, que tem uma sintaxe similar a `printf()` mas é dedicada a formatar strings de datas e horas. Seu primeiro argumento é o buffer onde a string formatada será armazenada, seguido pelo tamanho desse buffer, o formato desejado, e a estrutura `tm` que contém as informações da data e hora.

## Veja Também
- [Documentação do C sobre `time.h`](https://en.cppreference.com/w/c/chrono)
- [Tutorial C sobre manipulação de datas e horas](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Biblioteca de terceiros `date.h`](https://github.com/HowardHinnant/date)
