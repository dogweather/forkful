---
title:                "C: Apagando caracteres que correspondem a um padrão"
simple_title:         "Apagando caracteres que correspondem a um padrão"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que deletar caracteres que correspondem a um padrão?

Às vezes, em um programa de computador, precisamos lidar com cadeias de caracteres que podem incluir caracteres desnecessários ou indesejados. Em vez de percorrer manualmente cada caractere e excluí-los, podemos usar técnicas de programação para eliminar automaticamente caracteres que correspondem a um padrão específico.

## Como fazer isso em C

Para deletar caracteres que correspondem a um determinado padrão em C, podemos usar a função "strchr" da biblioteca de strings. Esta função procura uma ocorrência de um caractere especificado em uma string e, se encontrada, retorna um ponteiro para essa posição na string. Combinando essa função com um loop, podemos percorrer a string e excluir cada caractere que corresponde ao padrão.

Um exemplo de código em C seria:

```
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Isso é um exemplo de string com caracteres a mais";
    char *ptr;

    while ((ptr = strchr(text, 'a')) != NULL) {
        memmove(ptr, ptr + 1, strlen(ptr));
    }

    printf("%s", text);

    return 0;
}
```

Neste exemplo, estamos percorrendo a string e excluindo todos os caracteres 'a'. Para visualizar o resultado, a saída do programa seria:

```
Isso é um exemplo de string com crters A mis
```

## Profundidade técnica

A técnica usada no exemplo acima é conhecida como "exclusão por deslocamento". Isso envolve mover todos os caracteres à direita do caractere excluído um espaço para a esquerda, substituindo essencialmente o caractere indesejado.

No código, usamos a função "memmove" para fazer isso de forma eficiente. Esta função move um bloco de memória de uma posição para outra, garantindo que não haja sobreposição dos dados. Também usamos a função "strlen" para determinar o comprimento da parte da string que precisa ser movida.

## Veja também

- [Documentação da função strchr em C](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- [Mais sobre a função memmove em C](https://www.studytonight.com/c/function/standard-c-library-function-memmove) 
- [Tutorial de Strings em C](https://www.geeksforgeeks.org/strings-in-c-2/)