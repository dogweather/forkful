---
title:                "Transformar uma string em maiúsculas"
html_title:           "C: Transformar uma string em maiúsculas"
simple_title:         "Transformar uma string em maiúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que usar capitalização de string?

Um dos principais motivos para capitalizar uma string é garantir que a apresentação de dados seja consistente e legível para o usuário final. Isso é especialmente importante quando se trata de nomes próprios ou títulos.

## Como fazer?

Para capitalizar uma string em C, usamos a função `toupper()` da biblioteca string.h. Esta função transforma um caractere em sua forma maiúscula equivalente e pode ser aplicada a cada caractere da string usando um loop `for`. Veja o código a seguir:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char string[100];
   int i;

   //recebe a string do usuário
   printf("Insira uma string: ");
   scanf("%s", string);

   //loop para capitalizar cada caractere da string
   for(i = 0; i < strlen(string); i++) {
      string[i] = toupper(string[i]);
   }

   //imprime a string capitalizada
   printf("String capitalizada: %s", string);

   return 0;
}
```

Exemplo de entrada: `exemplo`
Saída: `EXEMPLO`

## Profundidade da Capitalização de String

Além da função `toupper()`, existem outras formas de capitalizar uma string em C. Por exemplo, podemos usar a função `strupr()` da mesma biblioteca, que realiza a capitalização direta sobre a string original. Também é possível implementar um algoritmo personalizado para capitalizar strings, que pode ser útil em situações específicas.

Outro aspecto importante a considerar é que a capitalização varia de acordo com o idioma. Em algumas línguas, apenas a primeira letra é maiúscula, enquanto em outras, todas as palavras são capitalizadas. Ao utilizar a função `toupper()`, é importante verificar qual o padrão de capitalização adequado para o idioma desejado.

## Veja também

- [Documentação da função toupper() em C](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [Documentação da função strupr() em C](https://www.tutorialspoint.com/c_standard_library/c_function_strupr.htm)
- [Guia prático de capitalização de strings em C](https://www.techiedelight.com/capitalize-string-c-program/)