---
title:                "Excluindo caracteres com correspondência de padrão"
html_title:           "C: Excluindo caracteres com correspondência de padrão"
simple_title:         "Excluindo caracteres com correspondência de padrão"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Há momentos em que precisamos limpar uma string, removendo determinados caracteres que não são necessários. Para isso, podemos usar a função `strpbrk()` em linguagem C para encontrar e remover caracteres que correspondam a um padrão específico.

## Como Fazer

Podemos usar a função `strpbrk()` de duas maneiras diferentes para remover caracteres com base em um padrão de correspondência. A primeira opção é fornecer uma string de caracteres que serão removidos da string original. Veja o exemplo abaixo:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char original[] = "Hello123World";
    char *pattern = "123";
    char *result;
    
    result = strpbrk(original, pattern);
    
    do {
        *result = ' ';
        result = strpbrk(result + 1, pattern);
    } while (result != NULL);
    
    printf("String limpa: %s", original);
    
    return 0;
}
```

Neste exemplo, usamos a função `strpbrk()` para encontrar a primeira ocorrência do padrão "123" na string `original`. Em seguida, usamos um loop `do while` para subsituir cada caractere encontrado pelo espaço em branco. O resultado final será a string "Hello World", onde os dígitos "123" foram removidos.

A segunda opção é fornecer uma string de caracteres que serão mantidos na string original. Veja o exemplo abaixo:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char original[] = "Hello123World";
    char *pattern = "HeoW";
    char *result;
    
    result = strpbrk(original, pattern);
    
    do {
        result++;
        *result = '\0';
        result = strpbrk(result + 1, pattern);
    } while (result != NULL);
    
    printf("String limpa: %s", original);
    
    return 0;
}
```

Neste exemplo, usamos a função `strpbrk()` para encontrar o primeiro caractere que não está presente na string `pattern`, neste caso, o caractere "l". Em seguida, usamos um loop `do while` para substituir cada caractere após o caractere encontrado até o final da string por um caractere nulo. O resultado final será a string "HeWo", onde apenas os caracteres "HeoW" foram mantidos.

## Deep Dive

É importante mencionar que a função `strpbrk()` só remove ou mantém caracteres a partir da primeira ocorrência do padrão especificado. Além disso, ela também pode ser usada para pesquisar por uma única ocorrência de um conjunto de caracteres, substituindo apenas o primeiro caractere encontrado.

Podemos também usar a função `strrchr()` para procurar a última ocorrência do padrão em vez da primeira. E para substituir todos os caracteres correspondentes na string, podemos usar a função `strcspn()`. Ambas as funções funcionam de maneira semelhante à `strpbrk()`, porém uma busca de trás para frente em vez de frente para trás.

## Veja Também

- [Referência da Função `strpbrk()` na Documentação do C](https://www.gnu.org/software/libc/manual/html_node/Finding-Tokens-in-a-String.html#index-strpbrk)
- [Outras funções de manipulação de strings em C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)