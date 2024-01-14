---
title:                "C: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Por que converter uma data em uma string em C?

Converter uma data em uma string é uma tarefa muito comum em programação C. Isso permite que os programadores exibam a data de forma legível para os usuários ou a armazenem em um banco de dados sem perder o formato original. Além disso, converter uma data em uma string permite uma maior flexibilidade no manuseio e manipulação de datas em um programa.

## Como fazer a conversão

Para converter uma data em uma string em C, é necessário usar a função `strftime` da biblioteca `time.h`. Essa função permite que você especifique o formato em que a data será exibida. Abaixo está um exemplo de código que mostra como usar a função `strftime` para converter uma data em uma string.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // cria uma estrutura de data
    time_t data_atual;
    struct tm *data;
    char string_data[50];

    // obtém a data atual do sistema
    data_atual = time(NULL);
    data = localtime(&data_atual);

    // converte a data em uma string com o formato desejado
    strftime(string_data, 50, "%d/%m/%Y", data);

    // imprime a string com a data formatada
    printf("Data atual: %s", string_data);

    return 0;
}
```

Exemplo de saída:

```
Data atual: 04/07/2021
```

Você pode alterar o formato na chamada da função `strftime` para atender às suas necessidades. Aqui estão alguns dos códigos de formatação mais comuns para datas:

- `%d` - dia do mês (01-31)
- `%m` - mês (01-12)
- `%Y` - ano com 4 dígitos
- `%y` - ano com 2 dígitos
- `%a` - abreviação do nome do dia da semana (Sun, Mon, Tue, etc.)
- `%A` - nome completo do dia da semana (Sunday, Monday, Tuesday, etc.)
- `%b` - abreviação do nome do mês (Jan, Feb, Mar, etc.)
- `%B` - nome completo do mês (January, February, March, etc.)

## Mergulho profundo

Além do formato da data, a função `strftime` também permite que você especifique o idioma em que a data será exibida. Isso é especialmente útil quando você precisa exibir datas em diferentes idiomas em um programa. Para fazer isso, basta usar a função `setlocale` da biblioteca `locale.h`, fornecendo o idioma desejado como argumento.

Outro detalhe importante a se ter em mente é que a função `strftime` retorna o número de caracteres escritos na string de saída. Isso pode ser muito útil quando você precisa manipular a string ou alocar memória para ela.

# Veja também
- [Documentação oficial da função `strftime`](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#Formatting-Calendar-Time)
- [Símbolos de formato para datas em C](https://www.cplusplus.com/reference/ctime/strftime/) (em inglês)