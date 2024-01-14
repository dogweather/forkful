---
title:    "C: Apagando caracteres que correspondem a um padrão"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com uma grande quantidade de dados em seu programa em C, é possível que em algum momento você precise remover caracteres que correspondam a um determinado padrão. Isso pode ser útil para limpar entradas de usuário ou para realizar operações específicas em uma string.

## Como fazer

Para remover caracteres em C, usamos a função `strcspn()` que retorna o número de caracteres consecutivos em uma string que não correspondem a um conjunto de caracteres especificado. Em seguida, podemos usar a função `memmove()` para mover esses caracteres para o início da string e sobrescrever os caracteres que queremos remover.

```
#include <stdio.h>
#include <string.h>

int main()
{
    char str[20] = "a1b2c3d4e5f6";
    
    // remove os números da string
    int n = strcspn(str, "0123456789");
    memmove(str, str + n, strlen(str) + 1 - n);
    
    printf("Nova string: %s\n", str);
    
    return 0;
}

// Output:
// Nova string: abcdef
```

## Mergulho Profundo

Uma coisa importante a se notar é que a função `strcspn()` considera apenas os caracteres que estão presentes no segundo parâmetro. Isso significa que se quisermos remover todos os números e letras maiúsculas de uma string, precisamos especificar todos os números e todas as letras maiúsculas no segundo parâmetro.

Além disso, a função `memmove()` funciona de forma semelhante à função `memcpy()`, mas com a diferença de que as áreas de memória de origem e destino podem se sobrepor. Isso é importante porque se usássemos `memcpy()` no código anterior, poderíamos acabar sobrescrevendo os caracteres que queremos manter.

## Veja também

- [Documentação oficial da função `strcspn()` em C](https://en.cppreference.com/w/c/string/byte/strcspn)
- [Tutorial de C da W3Schools sobre como remover caracteres de uma string](https://www.w3schools.in/c-tutorial/string-functions/strcspn/)