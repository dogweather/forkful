---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Artigo: Removendo Caracteres Correspondentes a um Padrão em C

## O Que & Por Quê?

Deleting caractere pelo padrão permite que você remova caracteres específicos de uma string, essencial quando você deseja extrair dados úteis ou limpar ruído. Isso ajuda a evitar erros relacionados à entrada do usuário, tornando o software mais resistente e eficiente.

## Como Fazer:

Veja um exemplo de como usar a função `strpbrk()`, que retorna um ponteiro para o personagem que corresponde a qualquer caracter de uma string:

```C
#include<stdio.h>
#include<string.h>

int main()
{
    char str1[40], str2[20];
    char *pos;
    
    printf("Digite a string inicial: ");
    fgets(str1, sizeof str1, stdin);
    
    printf("Digite os caracteres que deseja remover: ");
    fgets(str2, sizeof str2, stdin);
    
    while ((pos = strpbrk(str1, str2)) != NULL)
        memmove(pos, pos + 1, strlen(pos));
        
    printf("A string final é: %s\n", str1);
    
    return 0;
}
```

## Deep Dive

A função `strpbrk()` foi introduzida no padrão ISO C e, desde então, tem sido uma ferramenta útil para manipular strings em C. 
Como alternativa, os programadores também podem criar suas próprias funções personalizadas para excluir caracteres correspondentes a um padrão. No entanto, é sempre mais eficiente e seguro usar funções da biblioteca padrão sempre que possível.
Finalmente, sobre a implementação, `strpbrk()` busca na string o primeiro caractere que corresponde a qualquer caractere especificado na chave, retornando o ponteiro para esse caractere.

## Veja Também

Aqui estão alguns recursos adicionais se você deseja aprender mais:

1. Documentação oficial do GNU C Library: https://www.gnu.org/software/libc/manual/html_node/Search-Functions.html
2. Página do Tutorialspoint sobre a função `strpbrk()`: https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm
3. Página de referência Cplusplus sobre a função `strpbrk()`: http://www.cplusplus.com/reference/cstring/strpbrk/