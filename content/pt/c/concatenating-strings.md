---
title:                "Juntando strings"
html_title:           "C: Juntando strings"
simple_title:         "Juntando strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# O Que & Porquê?

Concatenar strings em programação é quando você combina duas ou mais strings em uma só. Isso é útil quando você precisa unir pedaços de texto para criar uma nova string. Os programadores fazem isso para facilitar o uso de textos mais complexos, como mensagens de erro ou conteúdo gerado dinamicamente.

# Como Fazer:

```C

#include <stdio.h>
#include <string.h> 

int main() {

   // Exemplo 1: Usando strcat()
   char str1[50] = "Olá, ";
   char str2[] = "mundo!";
   
   strcat(str1, str2);
   
   printf("%s", str1); // Output: Olá, mundo!
   
   // Exemplo 2: Usando sprintf()
   char str3[50];
   int idade = 25;
   
   sprintf(str3, "Eu tenho %d anos.", idade);
   
   printf("%s", str3); // Output: Eu tenho 25 anos.
   
   return 0;
}

```

# Mergulho Profundo:

Concatenar strings é uma técnica muito utilizada na programação, mas não é a única maneira de manipular textos. Existem outras opções, como o uso de ponteiros ou a utilização de bibliotecas de manipulação de strings. Além disso, é importante lembrar que, dependendo da linguagem de programação, a forma de concatenar strings pode variar. Por exemplo, em Java, a concatenação é feita com o operador "+".

# Ver Também:

https://www.geeksforgeeks.org/concatenate-string-integer-integer-strings-c/
https://www.techopedia.com/definition/25963/concatenate
https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm