---
title:                "C: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador iniciante ou experiente, provavelmente já se deparou com a necessidade de converter strings para letras minúsculas em algum momento. Há várias razões pelas quais isso pode ser importante em um projeto de programação, desde a formatação de dados até o processamento de entrada do usuário. Dominar essa habilidade pode facilitar muito o seu trabalho e melhorar a eficácia do seu código.

## Como Fazer

Há várias maneiras diferentes de converter uma string para letras minúsculas em C. Aqui, vamos mostrar dois exemplos utilizando a função `tolower()` da biblioteca padrão `<ctype.h>` e uma solução usando loops e condicionais.

#### Utilizando `tolower()`

````C
#include <stdio.h> 
#include <ctype.h> 

int main() { 
    char palavra[] = "CASA"; 

    for (int i = 0; palavra[i] != '\0'; i++) { 
        palavra[i] = tolower(palavra[i]); 
    } 

    printf("%s", palavra); 
    // saída será "casa"
    
    return 0; 
}
````

#### Com loops e condicionais

````C
#include <stdio.h> 

int main() {
    char palavra[] = "CASA"; 

    for (int i = 0; palavra[i] != '\0'; i++) {
        if (palavra[i] >= 'A' && palavra[i] <= 'Z') { 
            palavra[i] = palavra[i] + 32; 
        } 
    } 

    printf("%s", palavra); 
    // saída será "casa"
    
    return 0; 
}
````

Este é apenas um exemplo básico de como isso pode ser feito e é importante lembrar que há várias outras maneiras de alcançar o mesmo resultado. É importante entender a lógica por trás da conversão e escolher a abordagem que melhor se adapta ao seu projeto.

## Mergulho Profundo

Agora, vamos dar uma olhada mais profunda no processo de conversão de strings para letras minúsculas. Linguagens de programação usam códigos ASCII para representar caracteres. Para letras maiúsculas, os códigos variam de 65 a 90, enquanto que para letras minúsculas eles vão de 97 a 122. Portanto, simplesmente adicionando 32 ao código ASCII da letra maiúscula, podemos convertê-la para minúscula. No entanto, se a string contiver caracteres especiais ou acentos, pode ser necessário utilizar bibliotecas adicionais para garantir a correta conversão.

Outra coisa importante a se considerar é o impacto que a conversão pode ter no desempenho do seu código, especialmente se a string for longa. Em alguns casos, pode ser mais eficiente trabalhar com strings em maiúsculas e fazer a conversão apenas quando necessário.

## Veja Também

- [Conversão de Strings para Maiúsculas em C](https://www.programiz.com/c-programming/examples/uppercase-string)
- [Manipulação de Strings em C](https://www.geeksforgeeks.org/string-manipulation-in-c-without-using-library-function/)
- [Documentação da função `tolower()`](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)