---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Data e Strings: Uma Conversão Simples em C

## O Que & Por Quê?

A conversão de uma data para uma string em C envolve exibir a data num formato legível para o usuário, em forma de texto. Programadores fazem isso para facilitar a interação do usuário com as datas, tornando-as mais compreensíveis e utilizáveis.

## Como Fazer:

Vamos utilizar a função strftime() em C. Ela pega uma estrutura 'struct tm' e a converte para uma data em forma de texto.

```C
#include <time.h>
#include <stdio.h>

int main() {
    char buf[80];
    time_t t = time(NULL);
    struct tm *tm_info;
    
    tm_info = localtime(&t);

    strftime(buf, sizeof(buf), "%d-%m-%Y", tm_info);
    puts(buf);

    return 0;
}
```
A saída poderia ser algo como `12-05-2022`.

## Mergulho Profundo

Historicamente, a função de converter uma data para uma string em C tem-se mostrado essencial desde o advento do Unix, onde se tornou uma maneira padrão de apresentar datas para os usuários.

Em termos de alternativas, nós poderíamos usar gettimeofday() com ctime(), mas isso não nos dá o controle sobre o formato da data, como o strftime() nos dá.

Em termos de implementação, a função strftime() pega a estrutura 'struct tm' e um formato de string que define como a data será exibida, e retorna a data formatada como uma string.

## Veja Também:

Você pode encontrar mais informações sobre a função strftime() em sua página de manual no Linux [`man strftime`](http://man7.org/linux/man-pages/man3/strftime.3.html) ou na documentação oficial do cplusplus [`cpp referencia`](http://www.cplusplus.com/reference/ctime/strftime/). 

Para um entendimento mais aprofundado do manuseio de tempo e datas em C, dê uma olhada nesse tutorial [`tutorialspoint`](https://www.tutorialspoint.com/c_standard_library/time_h.htm).