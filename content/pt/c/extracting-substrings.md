---
title:                "C: Extraindo substrings"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em programação?

Ao escrever código em linguagem C, muitas vezes nos deparamos com a necessidade de extrair partes específicas de uma string. Isso pode ser útil em várias situações, como na validação de entradas do usuário ou na manipulação de dados em um banco de dados. Nesse sentido, a capacidade de extrair substrings é uma habilidade importante para os programadores.

## Como fazer isso em C?

Extrair substrings em C é uma tarefa relativamente simples. Primeiro, é necessário ter uma string da qual você quer extrair a substring. Suponha que temos a seguinte string:

```C
char str[] = "Extração de substrings em C";
```

Para extrair uma substring dessa string, é necessário especificar o índice de início e o tamanho da substring desejada. Por exemplo, se quisermos extrair a palavra "substrings", podemos fazer o seguinte:

```C
char substring[10];

// Índice de início da substring
int start = 14;

// Tamanho da substring desejada
int length = 10;

// Usando a função strncpy para extrair a substring
strncpy(substring, str + start, length);
```

Agora, a variável "substring" contém a substring "substrings". Podemos imprimir essa substring para verificar se ela foi extraída corretamente:

```C
printf("Substring: %s\n", substring);
```

O código acima produziria o seguinte resultado:

```
Substring: substrings
```

## Aprofundando-se na extração de substrings

Além da função `strncpy`, C também possui outras funções que podem ser úteis na extração de substrings, como `strchr` e `strtok`. É importante entender como essas funções funcionam e em quais situações elas podem ser mais adequadas.

Além disso, também é importante lembrar que, ao extrair substrings, devemos levar em consideração o tamanho da string original e do tamanho de substring desejado para evitar possíveis erros na execução do código.

## Veja também

- [Referência de Strings em C](https://www.codingame.com/playgrounds/14213/string-handling-in-c)
- [Documentação oficial da função strncpy](https://www.ibm.com/docs/en/zos/2.3.0?topic=functions-strncpy-store-string)
- [Tutorial de programação em C](https://www.devmedia.com.br/c-tutorial/)