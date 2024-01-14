---
title:                "C: Encontrando o comprimento de uma string."
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como os computadores conseguem determinar o tamanho de uma palavra ou frase? O que diferencia o número de caracteres em "casa" e "carro"? Neste artigo, vamos explorar como a linguagem de programação C pode nos ajudar a encontrar o comprimento de uma string.

## Como Fazer

Para encontrar o comprimento de uma string em C, usamos a função "strlen" da biblioteca "string.h". Esta função toma uma string como entrada e retorna o seu comprimento, ou seja, o número de caracteres. Veja um exemplo abaixo:

```C
#include <stdio.h>
#include <string.h>

int main()
{
	char palavra[10] = "casa";
	int comprimento = strlen(palavra);
	printf("A palavra \"%s\" tem %d caracteres.", palavra, comprimento);
	return 0;
}
```

O primeiro passo é incluir as bibliotecas "stdio.h" e "string.h" para podermos usar as funções "printf" e "strlen", respectivamente. Em seguida, declaramos uma variável do tipo "char" para armazenar a nossa string "casa". A seguir, chamamos a função "strlen", passando como parâmetro a variável "palavra" que criamos, e armazenamos o resultado na variável "comprimento". Finalmente, usamos a função "printf" para imprimir a palavra e o seu comprimento. A saída do programa será:

```
A palavra "casa" tem 4 caracteres.
```

## Mergulho Profundo

Você pode estar se perguntando como a função "strlen" é capaz de determinar o tamanho de uma string. O que acontece nos bastidores é que essa função percorre a string até encontrar o caractere nulo (representado pelo símbolo '\0'), que indica o final da string. A cada iteração do loop, ela incrementa um contador até que o caractere nulo seja encontrado, indicando o fim da string. O valor final do contador é então retornado como o comprimento da string.

É importante ressaltar que a função "strlen" conta apenas o número de caracteres até o caractere nulo, ou seja, não inclui esse caractere em si. Portanto, no exemplo acima, a palavra "casa" tem 4 caracteres, mas o tamanho da variável "palavra" é 5, já que o caractere nulo também ocupa um espaço na memória.

## Veja Também

- [Documentação oficial da função "strlen" em C](https://www.cplusplus.com/reference/cstring/strlen/)
- [Vídeo explicativo sobre a função "strlen"](https://www.youtube.com/watch?v=iCmWzW0EA0Y)
- [Exemplos de uso da função "strlen"](https://www.geeksforgeeks.org/strlen-function-in-c-cpp/)