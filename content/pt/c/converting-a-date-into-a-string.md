---
title:                "Convertendo uma data em uma string"
html_title:           "C: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e porque?

Converter uma data em uma string é uma tarefa comum na programação que permite que os programadores transformem uma data em um formato de texto legível. Isso facilita a visualização e a manipulação de datas em diferentes programas e sistemas.

Os programadores geralmente convertem datas em strings para facilitar a comunicação e o compartilhamento de dados com outros sistemas ou usuários. Também pode ser necessário para fins de formatação e visualização em interfaces de usuário.

## Como fazer:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // Obter a data atual
  time_t agora;
  time(&agora);

  // Converter a data em uma string
  char data_str[30];
  strftime(data_str, 30, "%d/%m/%Y", localtime(&agora));

  // Imprimir a data convertida
  printf("Hoje é %s\n", data_str);

  return 0;
}
```
Saída:
```
Hoje é 23/08/2021
```

## Mergulho profundo:

### Contexto histórico:
O uso de strings para representar datas remonta às primeiras linguagens de programação, onde as datas eram armazenadas como números inteiros. Com o avanço da programação e a necessidade de interação com interfaces de usuário, a conversão de datas em strings se tornou uma prática comum.

### Alternativas:
Embora seja a forma mais comum, converter uma data em uma string não é a única opção. Alguns programadores preferem trabalhar com estruturas de data prontas, enquanto outros usam funções de biblioteca que manipulam diretamente datas em seu formato original.

### Detalhes de implementação:
A biblioteca padrão C, <time.h>, fornece funções úteis para a manipulação de datas, como strftime() e localtime(). É importante lembrar de ajustar o formato da string de acordo com o ambiente em que ela será utilizada, já que diferentes países utilizam formatos de data diferentes.

## Veja também:

- [Manipulating time with C](https://www.lix.polytechnique.fr/~liberti/public/computing/prog/c/C/CONCEPT/date_time.html)
- [C library function - strftime()](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Date and Time Functions in C and C++](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)