---
title:    "C: Encontrando o comprimento de uma string"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por que encontrar o comprimento de uma string?

Ao trabalhar com strings em C, às vezes é necessário encontrar o comprimento da mesma. Isso pode ser útil em situações como validação de entrada do usuário ou alocação dinâmica de memória. Encontrar o comprimento de uma string é uma tarefa comum em programação C e é importante entender como fazê-lo corretamente.

## Como fazer

Existem algumas maneiras de encontrar o comprimento de uma string em C. A primeira é usando a função `strlen`, que retorna o comprimento de uma string passada como argumento. Vejamos um exemplo:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[20] = "Olá mundo!";
    int comprimento = strlen(str);
    printf("O comprimento da string '%s' é %d.", str, comprimento);
    return 0;
}

```

**Saída:**

``` 
O comprimento da string 'Olá mundo!' é 10.
```

Além disso, é possível encontrar o comprimento de uma string usando um loop até que o caractere nulo (`\0`) seja encontrado, indicando o final da string. Vejamos um exemplo:

```C
#include <stdio.h>

int main() {
    char str[20] = "Olá mundo!";
    int comprimento = 0;
    int i = 0;
    while (str[i] != '\0') {
        comprimento++;
        i++;
    }
    printf("O comprimento da string '%s' é %d.", str, comprimento);
    return 0;
}

```

**Saída:**

``` 
O comprimento da string 'Olá mundo!' é 10.
```

## Deep Dive

Ao trabalhar com strings em C, é importante lembrar que o caractere nulo (`\0`) é usado para indicar o final de uma string. Ele é adicionado automaticamente no final das strings, portanto, quando usamos a função `strlen`, ela não inclui o caractere nulo no comprimento total da string. Isso significa que o comprimento retornado por `strlen` é o número de caracteres reais na string, excluindo o caractere nulo.

Além disso, é importante lembrar que as strings em C são arrays de caracteres, portanto, o tamanho máximo de uma string será determinado pelo tamanho máximo de um array. Se você tentar criar uma string que exceda esse tamanho, poderá ocorrer um erro na execução do programa.

## Veja também

- [Referência da função `strlen` na documentação da biblioteca C](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_72/rtref/strlen.htm)
- [Guia completo sobre strings em C](https://www.studytonight.com/c/string-in-c.php)
- [Tutorial de alocação dinâmica de memória em C](https://www.geeksforgeeks.org/dynamic-memory-allocation-in-c-using-malloc-calloc-free-and-realloc/)