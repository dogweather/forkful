---
title:    "C++: Obtendo a data atual"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que Obter a Data Atual em C++

Se você é um programador iniciante ou experiente, é provável que em algum momento precise obter a data atual em seu código. Isso pode ser útil para realizar cálculos com datas, registrar eventos ou para qualquer outra tarefa que envolva o gerenciamento de tempo. Felizmente, obter a data atual em C++ é relativamente simples e neste artigo vou mostrar como fazer isso.

## Como Fazer

Para obter a data atual em C++, primeiro precisamos incluir a biblioteca "ctime". Esta biblioteca contém a função "time", que nos permite obter a data atual em segundos a partir de uma data de referência, conhecida como epoch. Nós podemos então converter esses segundos em uma estrutura de dados chamada "tm", que contém informações como dia, mês, ano, hora, minuto e segundo. Finalmente, podemos usar essa estrutura para imprimir a data de maneira formatada.

Aqui está um exemplo de código que mostra como obter e formatar a data atual em C++:

```C++
#include <iostream>
#include <ctime>

int main(){
    // Obtém a data atual em segundos
    time_t now = time(0);

    // Converte em uma estrutura de dados "tm"
    tm *current = localtime(&now);

    // Imprime a data formatada
    std::cout << "Data atual: " << current->tm_mday << "/" << current->tm_mon + 1 << "/" << current->tm_year + 1900;

    return 0;
}
```

A saída deste código seria algo parecido com isso:

```
Data atual: 13/01/2022
```

## Mergulho Profundo

Agora que já sabemos como obter a data atual em C++, vamos entender um pouco mais sobre a função "time" e a estrutura "tm". A função "time" retorna o número de segundos passados desde 1 de janeiro de 1970, também conhecido como epoch. Isso é conhecido como um timestamp e é uma maneira comum de armazenar e manipular datas em programação.

A estrutura "tm" é usada para armazenar informações de data e hora em C++. Ela contém os seguintes campos:

- tm_sec (segundo)
- tm_min (minuto)
- tm_hour (hora)
- tm_mday (dia do mês)
- tm_mon (mês)
- tm_year (ano)
- tm_wday (dia da semana)
- tm_yday (dia do ano)
- tm_isdst (horário de verão)

Vale ressaltar que os campos "tm_mon" e "tm_year" começam em 0, portanto, é necessário adicionar 1 a eles ao imprimi-los ou usá-los em cálculos.

## Veja Também

- [Documentação da biblioteca ctime em C++](https://www.cplusplus.com/reference/ctime/)
- [Função "time" na documentação da biblioteca ctime](https://www.cplusplus.com/reference/ctime/time/)
- [Informações sobre a estrutura "tm" em C++](https://www.cplusplus.com/reference/ctime/tm/)