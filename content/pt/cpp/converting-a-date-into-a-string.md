---
title:                "Convertendo uma data em uma string."
html_title:           "C++: Convertendo uma data em uma string."
simple_title:         "Convertendo uma data em uma string."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando em um projeto de programação que envolve datas, provavelmente precisará converter as datas em uma string em algum momento. Isso pode ser útil para imprimir a data em um arquivo de log ou para exibir a data formatada em uma interface de usuário.

## Como Fazer

Para converter uma data em uma string em C++, podemos usar a função `strftime` da biblioteca `ctime`. Aqui está um exemplo de código que mostra como fazer isso:

```C++
#include <iostream>
#include <ctime>

int main() {
    // Criando uma variável de data
    std::tm date = {0, 0, 0,     // Segundos, minutos, horas
                    1, 0, 2021}; // Dia, mês, ano (usamos 0 para os campos que não precisamos)

    // Criando um buffer de tamanho suficiente para armazenar a string
    char buffer[80];

    // Usando strftime para converter a data em uma string
    strftime(buffer, 80, "A data de hoje é: %d/%m/%Y", &date);

    // Imprimindo a string
    std::cout << buffer << std::endl;

    return 0;
}

/* Saída:
A data de hoje é: 01/01/2021
*/

```

Neste exemplo, criamos uma variável de data e usamos a função `strftime` para convertê-la em uma string com o formato desejado. O primeiro parâmetro da função é um array de caracteres que irá armazenar a string, o segundo parâmetro é o tamanho do array e o terceiro é a formatação que queremos para a string. O último parâmetro é a variável de data que queremos converter.

## Deep Dive

A função `strftime` é muito versátil e permite que você formate a data de várias maneiras diferentes. Aqui estão alguns dos marcadores de formato mais comuns para usar na formatação:

- `%d` - dia do mês (de 01 a 31)
- `%m` - mês (de 01 a 12)
- `%Y` - ano com quatro dígitos
- `%y` - ano com dois dígitos
- `%H` - hora no formato 24 horas (de 00 a 23)
- `%I` - hora no formato 12 horas (de 01 a 12)
- `%M` - minutos (de 00 a 59)
- `%S` - segundos (de 00 a 59)
- `%p` - AM ou PM (apenas disponível para o formato de 12 horas)

Você pode utilizar esses marcadores de formato para criar a string de data que melhor atende às suas necessidades.

## Veja Também

- [Documentação da função `strftime`](https://www.cplusplus.com/reference/ctime/strftime/)
- [Tutorial sobre manipulação de datas em C++](https://www.freecodecamp.org/news/how-to-work-with-dates-in-cpp-customize-your-handling-of-datetime-values/)
- [Artigo sobre formatação de datas em C++](https://www.learncpp.com/cpp-tutorial/formatting-output-printf-and-iostream/)