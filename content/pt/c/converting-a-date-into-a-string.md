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

## Por que
Muitas vezes, em programação, precisamos lidar com datas e, em alguns casos, precisamos convertê-las em formato de string para que possamos manipulá-las e exibi-las de forma mais amigável para o usuário. Por isso, aprender a converter uma data em string é um conhecimento importante para qualquer programador em C.

## Como fazer
A conversão de uma data em string pode ser feita de diversas formas, mas neste artigo vamos abordar a utilização da função `strftime()`, que já está presente na biblioteca padrão de C.

Para começar, precisamos incluir a biblioteca `time.h` em nosso código. Em seguida, podemos utilizar a função `strftime()` com os seguintes parâmetros: a string que será preenchida com a data, o tamanho máximo dessa string, o formato desejado e a data que será convertida.

Vamos ver um exemplo prático disso:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char data[50];
    time_t agora = time(NULL);

    /* Convertendo a data atual em string no formato dd/mm/aaaa */
    strftime(data, sizeof(data), "%d/%m/%Y", localtime(&agora));

    printf("A data atual é: %s\n", data);

    return 0;
}
```
A saída desse código será algo como: `A data atual é: 30/07/2021`.

Podemos personalizar o formato da data conforme nossa necessidade, utilizando os identificadores disponíveis na documentação da função `strftime()`. Por exemplo, para mostrar a data no formato "Dia da semana, dia de mês de ano", podemos utilizar o seguinte código:

```c
strftime(data, sizeof(data), "%A, %d de %B de %Y", localtime(&agora));
```
E a saída seria algo como: `Sexta-feira, 30 de julho de 2021`.

## Mergulho profundo
A função `strftime()` é bastante versátil e nos permite realizar diversas formatações ao converter uma data em string. Além dos identificadores básicos de data, podemos utilizar os de hora, fuso horário e até mesmo localização.

Também é importante lembrar que quando utilizamos a função `strftime()`, a formatação da data será baseada no local em que o programa está sendo executado. Por isso, é recomendado utilizar a função `setlocale()` para definir o local correto caso seja necessário.

## Veja também
- [Documentação oficial da função strftime()](https://www.cplusplus.com/reference/ctime/strftime/)
- [Tutorial sobre impressão de datas em C](https://www.geeksforgeeks.org/printing-dates-c-program/)
- [Exemplo prático de utilização da função strftime()](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)