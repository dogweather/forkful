---
title:    "C: Extração de subcadeias de caracteres"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings é importante?

Uma substring é uma parte de uma string maior. Em programação, é comum precisar extrair certas informações de uma string específica. Isso pode ser útil para fazer manipulações de dados ou para analisar informações em um determinado formato. Extrair substrings é uma tarefa comum em programação e pode ser muito útil em diversas situações.

## Como extrair substrings em C

Em C, existem algumas funções que podem ser utilizadas para extrair substrings de uma string maior. A seguir, vamos ver alguns exemplos de código e sua saída correspondente.

```C
#include<stdio.h>

int main() {
    char str[] = "Programação em C";
    char sub[5];
    
    // Extraindo os primeiros 5 caracteres da string
    strncpy(sub, str, 5);
    printf("A substring é: %s \n", sub);
    
    // Extraindo a parte da string iniciando na posição 7
    char *ptr = str + 7;
    printf("A substring é: %s \n", ptr);
    
    return 0;
}

```
Saída:
```
A substring é: Progr
A substring é: em C
```

Nesse exemplo, estamos utilizando a função `strncpy()` para copiar os primeiros 5 caracteres da variável `str` para a variável `sub`. Também estamos utilizando a forma de ponteiro para extrair a parte da string iniciando na posição 7.

## Explorando mais sobre a extração de substrings

Além da função `strncpy()`, existem outras funções que podem ser utilizadas para extrair substrings em C. Uma delas é a função `strtok()`, que pode ser utilizada para dividir uma string em várias partes, delimitadas por um caractere específico. Também é possível utilizar a função `strchr()`, que permite encontrar o primeiro caractere específico dentro de uma string e utilizar sua posição como ponto de início para extrair a substring.

A extração de substrings também é uma tarefa importante em expressões regulares, uma ferramenta poderosa para manipulação de strings. É possível utilizar funções como `regcomp()` e `regexec()` para encontrar padrões dentro de uma string e extrair as informações desejadas.

## Veja também

- [Tutoriais de C da DevMedia](https://www.devmedia.com.br/tutoriais/c/)
- [Documentação do C na linguagem C](https://docs.microsoft.com/pt-br/dotnet/csharp/language-reference/built-in-types/string)
- [Tutorial de expressões regulares em C](https://www.tutorialspoint.com/c_standard_library/c_function_regexec.htm)