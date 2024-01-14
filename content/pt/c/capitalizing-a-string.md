---
title:    "C: Capitalizando uma string"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que capitalizar uma string em C?

Capitalizar uma string em C significa transformar todas as letras minúsculas em maiúsculas. Este processo pode ser útil em várias situações, como por exemplo, padronizar nomes de arquivos ou dados de entrada.

# Como fazer?

Em C, podemos capitalizar strings usando a função `toupper()` da biblioteca `ctype.h`. O código abaixo mostra um exemplo simples de como capitalizar uma string:

```C
#include <stdio.h>
#include <ctype.h> 

int main() 
{ 
    char string[] = "ola, mundo!"; 
    
    // Loop para percorrer cada caractere da string
    for(int i = 0; string[i] != '\0'; i++) 
    { 
        // Converte o caractere para maiúsculo 
        string[i] = toupper(string[i]); 
    } 
    
    printf("String capitalizada: %s", string); 
    
    return 0; 
}
```
Output:

`String capitalizada: OLA, MUNDO!`

Também podemos usar uma função personalizada para capitalizar uma string, como mostrado abaixo:

```C
#include <stdio.h> 

// Função que capitaliza uma string 
void capitalize(char *string) 
{ 
    // Loop para percorrer cada caractere da string 
    for(int i = 0; string[i] != '\0'; i++) 
    { 
        // Verifica se o caractere é uma letra minúscula 
        if(string[i] >= 'a' && string[i] <= 'z') 
        { 
            // Converte o caractere para maiúsculo 
            string[i] = 'A' + (string[i] - 'a'); 
        } 
    } 
}

int main() 
{ 
    char string[] = "hello, world!"; 
    
    capitalize(string); 
    
    printf("String capitalizada: %s", string); 
    
    return 0; 
} 
```
Output:

`String capitalizada: HELLO, WORLD!`

# Uma análise mais aprofundada

A função `toupper()` é uma função de biblioteca bastante útil para capitalizar strings e está definida em `<ctype.h>`. Ela recebe um caractere como argumento e retorna o mesmo caractere, mas em maiúsculo. No entanto, esta função só funciona para caracteres individuais e não para strings inteiras.

É por isso que, em nossos exemplos, usamos loops para percorrer cada caractere da string e, em seguida, aplicar a função `toupper()` a cada caractere individualmente. Além disso, certifique-se de que o caractere esteja em minúsculo antes de aplicar a função, caso contrário, a função retornará o mesmo caractere sem modificação.

# Veja também

- [Documentação da função toupper() em C](https://www.geeksforgeeks.org/toupper-function-in-c/)
- [Mais exemplos de capitalização de strings em C](https://www.tutorialspoint.com/c-programs-to-capitalize-the-first-letter-of-each-word-in-a-string)