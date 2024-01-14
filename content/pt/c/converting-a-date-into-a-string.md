---
title:                "C: Convertendo uma data em uma string"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Por que converter uma data em uma string?

Converter uma data em uma string é uma tarefa comum em muitos programas de computador. Isso ocorre porque as datas são armazenadas em formatos específicos para cálculos e comparações, mas muitas vezes é necessário apresentá-las em um formato mais amigável ao usuário.

# Como fazer

## Bibliotecas necessárias
Para converter uma data em uma string, você precisa incluir a biblioteca `time.h` em seu código.

## Exemplo de código
Vamos usar a função `strftime()` para converter uma data em uma string. Esta função aceita dois argumentos: o formato da string de saída e uma estrutura `struct tm` que armazena a data e a hora.

```
#include <stdio.h>
#include <time.h>

int main()
{
    // Obtém a data atual
    time_t now = time(NULL);
    // Converte para uma estrutura de tempo
    struct tm *local = localtime(&now);
    // Define o tamanho máximo da string de saída
    char buffer[80];
    // Converte a data para uma string no formato dia/mês/ano
    strftime(buffer, 80, "%d/%m/%Y", local);

    // Imprime a data na tela
    printf("Data atual: %s\n", buffer);

    return 0;
}
```
Output:
```
Data atual: 12/04/2021
```

# Mergulho profundo

Existem muitos formatos diferentes que podem ser usados ​​para converter uma data em uma string, como o formato de 24 horas, o formato AM/PM e a apresentação do ano com dois ou quatro dígitos. Você pode escolher o formato que melhor se adapta ao seu programa usando uma combinação de caracteres especiais na função `strftime()`. Alguns exemplos são `%H` para horas em formato 24 horas, `%I` para horas em formato 12 horas, `%p` para AM/PM e `%y` para o ano com dois dígitos.

Além disso, você também pode especificar o idioma da string de saída usando a opção `%b` ou `%B` para o nome do mês abreviado ou completo, respectivamente.

# Veja também

- [Documentação da função strftime](https://www.cplusplus.com/reference/ctime/strftime/)
- [Tutorial de C da W3Schools](https://www.w3schools.in/c-tutorial/date-time/)
- [Artigo sobre conversão de data em string em C](https://www.geeksforgeeks.org/convert-date-string-using-c/)