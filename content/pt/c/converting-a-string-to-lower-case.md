---
title:    "C: Convertendo uma string para minúsculas."
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que

Em programação, muitas vezes nos deparamos com a necessidade de manipular strings de texto. Em alguns casos, precisamos converter essas strings para letras minúsculas. Isso pode ser útil em situações como validar senhas em maiúsculas e minúsculas ou tornar a entrada de usuário mais consistente.

## Como Fazer

Para converter uma string para minúsculas em C, podemos usar a função `tolower()` da biblioteca `ctype.h`. Veja o exemplo abaixo:

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
  char texto[] = "EXEMPLO DE STRING";
  int i = 0;

  printf("Texto original: %s\n", texto);

  while (texto[i])
  {
    texto[i] = tolower(texto[i]);
    i++;
  }

  printf("Texto convertido para minúsculas: %s\n", texto);
  
  return 0;
}
```

Este código irá percorrer cada caractere da string e usar a função `tolower()` para convertê-lo para minúscula. O resultado será:

```
Texto original: EXEMPLO DE STRING
Texto convertido para minúsculas: exemplo de string
```

## Mergulho Profundo

Além da função `tolower()`, também podemos usar a função `strlwr()` da biblioteca `string.h` para converter uma string para minúsculas. A diferença é que a função `strlwr()` altera diretamente a string original, enquanto a `tolower()` retorna uma nova string com as letras minúsculas. Além disso, é importante ter em mente que essas funções não alteram caracteres acentuados.

Outro ponto importante é que, ao manipular strings, devemos sempre garantir que haja espaço suficiente alocado para a string convertida. Um erro comum é tentar alterar diretamente a string original, mas sem ter alocado espaço suficiente, o que pode resultar em comportamento inesperado.

## Veja Também

- [Função tolower()](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Função strlwr()](https://www.tutorialspoint.com/c_standard_library/c_function_strlwr.htm)
- [Manipulação de strings em C](https://www.geeksforgeeks.org/string-manipulation-in-c-with-examples/)
- [Biblioteca C padrão](https://www.ibm.com/docs/pt-br/openshift-enterprise/3.11.0?topic=mi-6-algoritmos-pathxl)
- [Tutorial C para iniciantes](https://www.learn-c.org/)