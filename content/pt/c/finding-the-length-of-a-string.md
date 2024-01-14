---
title:                "C: Encontrando o comprimento de uma sequência de caracteres"
simple_title:         "Encontrando o comprimento de uma sequência de caracteres"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por que encontrar o comprimento de uma string é importante?

Encontrar o comprimento de uma string é uma tarefa comum em programação C. É importante porque ajuda a determinar o espaço necessário para armazenar uma string e a realizar operações como adicionar ou remover caracteres. Além disso, é uma habilidade essencial para lidar com strings de maneira eficiente e evitar erros de memória.

# Como encontrar o comprimento de uma string em C?

Em C, uma string é uma sequência de caracteres terminada por um caractere nulo '\0'. O comprimento de uma string pode ser determinado contando o número de caracteres antes do caractere nulo. Para isso, podemos usar a função `strlen()` da biblioteca `<string.h>`, que retorna o comprimento de uma string em bytes.

```
#include <stdio.h>
#include <string.h>

int main() {
  char string[50] = "Hello World"; // declarando e inicializando uma string
  int comprimento = strlen(string); // calculando o comprimento da string
  printf("O comprimento da string é: %d", comprimento); // mostrando o comprimento na tela
  return 0;
}
```
**Saída:** O comprimento da string é: 11

No exemplo acima, declaramos uma string "Hello World" com 11 caracteres e usamos a função `strlen()` para calcular e exibir seu comprimento na tela.

# Aprofundando no assunto

Ao trabalhar com strings, é importante ter em mente que o caractere nulo '\0' é considerado parte da string. Portanto, o comprimento de uma string vazia é 1, pois ela contém apenas o caractere nulo.

Além disso, é importante lembrar que o tamanho máximo de uma string em C é limitado pela quantidade de memória disponível, pois a string será armazenada na memória RAM.

Uma maneira alternativa de encontrar o comprimento de uma string é usando um loop `while` para percorrer a string até encontrar o caractere nulo. Isso pode ser útil para entender como a função `strlen()` funciona nos bastidores.

```
#include <stdio.h>

int main() {
  char string[50] = "Hello World";
  int comprimento = 0; // inicializando a variável comprimento com 0
  while (string[comprimento] != '\0') { // percorrendo a string até encontrar o caractere nulo
    comprimento++; // incrementando o valor de comprimento a cada iteração
  }
  printf("O comprimento da string é: %d", comprimento);
  return 0;
}
```
**Saída:** O comprimento da string é: 11

# Ver também

- [Tutorial de Strings em C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Documentação da função `strlen()`](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Exemplos de manipulação de strings em C](https://www.programiz.com/c-programming/c-strings)