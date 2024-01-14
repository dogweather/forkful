---
title:    "C: Obtendo a data atual."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Porque Obter a Data Atual em Programação é Importante

Quando estamos construindo um programa, muitas vezes precisamos trabalhar com diferentes datas e horários. Isso pode ser necessário para rastrear eventos, gerar relatórios, ou até mesmo fazer operações matemáticas básicas. Portanto, obter a data atual em um programa pode ser uma tarefa importante e útil.

# Como Obter a Data Atual em Programação

A linguagem de programação C possui uma biblioteca padrão que nos permite obter a data atual de forma rápida e eficiente. Para isso, podemos utilizar as funções `time()` e `localtime()`, que nos permitem acessar a informação da data atual em uma estrutura de dados `struct tm`.

```
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Obtendo a data atual
    time_t currentTime = time(NULL); 
    // Convertendo para estrutura de dados
    struct tm *local = localtime(&currentTime);
    // Imprimindo a data no formato desejado
    printf("A data atual é: %d/%d/%d\n", local->tm_mday, local->tm_mon + 1, local->tm_year + 1900);
}
```

**Saída:**

```
A data atual é: 3/10/2021
```

# Detalhando a Obtenção da Data Atual

Além de utilizar as funções já mencionadas, também é possível obter informações mais específicas sobre a data atual, como o dia da semana, o horário e até mesmo o fuso horário. Isso pode ser útil quando precisamos exibir informações mais precisas em nosso programa.

Outra coisa importante a ser considerada é que a biblioteca `time.h` possui diversas outras funções relacionadas a data e hora, como `asctime()`, que converte a estrutura `struct tm` para uma string formatada, ou `mktime()`, que converte a data em um valor do tipo `time_t`. Vale a pena explorar essas funções para um melhor entendimento e utilização da biblioteca.

# Veja Também

- [Documentação da biblioteca `time.h` em português](https://pt.cppreference.com/w/c/chrono)
- [Tutorial sobre data e hora em C](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [Outras funções da biblioteca `time.h`](https://www.geeksforgeeks.org/c-header-time-h-with-examples/)