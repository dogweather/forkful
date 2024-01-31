---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:41:45.701311-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deletar caracteres que correspondem a um padrão em C significa procurar certos caracteres numa string e removê-los. Programadores fazem isso para limpar entradas de dados, validar informações ou preparar strings para serem processadas mais adiante.

## How to:

Vamos usar a função `strpbrk` da biblioteca padrão de C para procurar os caracteres indesejados e a função `strcpy` para removê-los.

```c
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *found = str;
    while (*found) {
        found = strpbrk(found, pattern);
        if (!found) {
            break;
        }
        strcpy(found, found + 1);
    }
}

int main() {
    char texto[] = "Exemplo: Eliminar 123 os dígitos.";
    const char *padrao = "1234567890";

    printf("Original: %s\n", texto);
    delete_pattern(texto, padrao);
    printf("Modificado: %s\n", texto);

    return 0;
}
```

Saída esperada:

```
Original: Exemplo: Eliminar 123 os dígitos.
Modificado: Exemplo: Eliminar  os dígitos.
```

## Deep Dive

A função `strpbrk` existe desde a linguagem C padrão ANSI C, adotada em 1989. Ela é uma das várias funções de manipulação de strings disponíveis no `<string.h>`. Uma alternativa seria usar expressões regulares com a biblioteca `regex.h`, mas isso pode ser exagero para casos simples.

Quanto aos detalhes de implementação, `strpbrk` busca a primeira ocorrência de qualquer um dos caracteres fornecidos no padrão, enquanto `strcpy` copia strings, possibilitando sobrepor o caractere indesejado.

Em nossa abordagem direta, simplesmente sobrescrevemos os caracteres que queremos remover ao copiar o resto da string um caractere para trás, usando o `strcpy`. É uma técnica eficiente para strings menores, mas pode não ser ideal para strings muito grandes devido à operação de cópia envolvida a cada remoção.

## See Also

- Documentação da função `strpbrk`: https://www.cplusplus.com/reference/cstring/strpbrk/
- Tutorial sobre manipulação de strings em C: https://www.tutorialspoint.com/cprogramming/c_strings.htm
- Introdução a expressões regulares em C: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
