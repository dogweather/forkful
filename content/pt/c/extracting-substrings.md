---
title:                "C: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair subcadeias?

A extração de subcadeias é uma técnica muito útil e comumente utilizada em programação. Ela permite que o programador extraia uma parte específica de uma string, que pode ser usada posteriormente para diferentes fins. Por exemplo, se você quiser pegar apenas o sobrenome de uma pessoa em um banco de dados, a extração de subcadeias pode ser útil. Além disso, essa técnica também pode ser usada para verificar se uma determinada palavra está presente em uma string maior, entre outras possibilidades.

## Como fazer a extração de subcadeias em C

Para extrair subcadeias em C, é necessário utilizar a função `strncpy()` da biblioteca `string.h`. Essa função recebe três parâmetros: a string na qual será feita a extração, o índice inicial da subcadeia e o tamanho da subcadeia. Veja um exemplo de código abaixo:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char str[] = "Este é um exemplo de string";
   char subc[10];

   // utilizando strncpy para extrair uma subcadeia de tamanho 10
   strncpy(subc, str + 11, 10);
   subc[10] = '\0';

   printf("Subcadeia extraída: %s\n", subc);
   
   return 0;
}
```

O código acima irá extrair a subcadeia "exemplo de" da string "Este é um exemplo de string". Note que é necessário adicionar o caractere nulo (`'\0'`) ao final da subcadeia para indicar seu término.

O resultado esperado da execução do código acima será:

```
Subcadeia extraída: exemplo de
```

## Mergulho mais profundo na extração de subcadeias

Além da função `strncpy()`, existem várias outras maneiras de extrair subcadeias em C. Algumas delas são:

- Utilizar `strncat()` para juntar duas strings e em seguida utilizar `strncpy()` para extrair a subcadeia desejada.
- Utilizar a técnica de ponteiros para percorrer a string e copiar a subcadeia para outra variável.
- Utilizar `sscanf()` para ler a string e armazenar a subcadeia em uma variável.

É importante lembrar que ao extrair uma subcadeia, é necessário garantir que a string tem um tamanho suficiente para armazenar a subcadeia desejada.

## Veja também
- [Documentação oficial da função `strncpy()` em C](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Tutorial sobre a extração de subcadeias em C](https://www.geeksforgeeks.org/c-program-extracting-substrings-given-string/)
- [Outras funções úteis para manipulação de strings em C](https://www.geeksforgeeks.org/string-handling-library-functions-c/)