---
title:                "C: Concatenando strings"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que usar a concatenação de strings em C?

A concatenação de strings é uma das habilidades essenciais que todo programador em C deve ter. É uma técnica útil para juntar duas ou mais strings e criar uma única string. Isso pode ser útil em situações em que você precisa combinar um nome e um sobrenome, criar uma mensagem personalizada ou até mesmo construir um URL completo a partir de várias partes.

Agora, vamos dar uma olhada em como exatamente podemos realizar a concatenação de strings em C.

## Como fazer a concatenação de strings em C

Aqui está um exemplo básico de como podemos usar a função `strcat()` para juntar duas strings em C:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char string1[] = "Olá, ";
    char string2[] = "mundo!";
    char resultado[50];

    strcat(resultado, string1);
    strcat(resultado, string2);

    printf("%s", resultado);

    return 0;
}

```

Este código irá produzir a seguinte saída:

```
Olá, mundo!
```

Podemos ver que ao usar a função `strcat()`, as duas strings foram unidas em uma única string que foi armazenada na variável `resultado`. Isso pode ser muito útil quando precisamos criar uma frase ou mensagem personalizada em nosso programa.

Também é importante notar que, antes de usar a função `strcat()`, é necessário inicializar o array `resultado` com um tamanho suficientemente grande para armazenar as duas strings concatenadas. Caso contrário, podemos encontrar problemas de buffer overflow.

## Descobrindo mais sobre concatenação de strings

Como mencionado anteriormente, a concatenação de strings é uma habilidade essencial em C e, portanto, é importante que os programadores entendam como usá-la corretamente. A função `strcat()` é apenas uma das muitas funções que podem ser usadas para concatenar strings em C. É altamente recomendável explorar outras funções, como `strcpy()`, `strncat()`, `sprintf()` e `strdup()` para ter uma compreensão completa deste tópico.

Além disso, é importante lembrar que a concatenação de strings também pode ser feita manualmente, percorrendo cada caractere de uma string e adicionando-os à outra string. No entanto, esse método é um pouco mais trabalhoso e pode ser ineficiente em termos de desempenho.

## Veja também

- [Documentação da função `strcat()`](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Outras funções úteis de string em C](https://www.guru99.com/c-strings.html)
- [Tutorial completo sobre strings em C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)