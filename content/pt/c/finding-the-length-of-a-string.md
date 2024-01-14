---
title:    "C: Encontrando o comprimento de uma string"
keywords: ["C"]
---

{{< edit_this_page >}}

## Porquê

Encontrar o comprimento de uma string é uma tarefa comum e importante na programação em C. Isso permite que você saiba quantos caracteres uma string possui, o que é útil para várias aplicações, como validação de entrada do usuário ou manipulação de texto.

## Como Fazer

Para encontrar o comprimento de uma string em C, você pode usar a função `strlen()` da biblioteca `string.h`. Esta função leva uma string como parâmetro e retorna a quantidade de caracteres presentes nela. Veja um exemplo de código abaixo:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char minha_string[] = "Olá, mundo!";
    int comprimento = strlen(minha_string);
    
    printf("A string possui %d caracteres.", comprimento);
    return 0;
}
```

A saída deste código será:

```
A string possui 12 caracteres.
```

Você também pode encontrar o comprimento de uma string manualmente, iterando sobre cada caractere até encontrar o caractere nulo (`\0`), que indica o final da string. Confira o exemplo abaixo:

```C
#include <stdio.h>

int main() {
    char minha_string[] = "Olá, mundo!";
    int comprimento = 0;
    
    // Itera sobre a string até encontrar o caractere nulo
    while(minha_string[comprimento] != '\0') {
        comprimento++;
    }
    
    printf("A string possui %d caracteres.", comprimento);
    return 0;
}
```

A saída será a mesma que a do primeiro exemplo.

## Deep Dive

A função `strlen()` na verdade conta a quantidade de caracteres até o caractere nulo, por isso ela não contabiliza o próprio caractere nulo, mas apenas os caracteres que o precedem. Além disso, é importante ressaltar que essa função não conta apenas letras, mas sim qualquer caractere presente na string, incluindo espaços em branco e caracteres especiais.

Outra coisa a ser considerada é que o tipo de dado utilizado para armazenar o comprimento da string é o `int`, portanto, a quantidade de caracteres que uma string pode ter é limitada pelo tamanho máximo de um `int`. Se a sua string for maior que isso, ela será cortada e o comprimento retornado será um valor incorreto.

## Veja Também

- [Como usar strings em C](https://www.programiz.com/c-programming/c-strings)
- [Função `strlen()` da biblioteca `string.h`](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Tutorial completo de C](https://www.cprogramming.com/tutorial/c-tutorial.html)