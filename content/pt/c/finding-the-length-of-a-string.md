---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente está se perguntando por que alguém se preocuparia em descobrir o comprimento de uma string em C. Bem, existem diversas situações em que essa informação pode ser útil. Por exemplo, você pode precisar delimitar o tamanho de uma string para alocar memória suficiente, ou pode estar criando uma função que verifica se uma entrada do usuário excede um certo limite de tamanho.

Sabemos que o C é uma linguagem de programação poderosa, mas também pode ser um pouco intimidante. Se você ainda está aprendendo, é normal se sentir sobrecarregado com alguns conceitos. Mas não se preocupe, neste artigo vamos explorar como descobrir o comprimento de uma string em C de uma forma simples e clara.

## Como fazer

Antes de começar, é importante lembrar que uma string em C é simplesmente um array de caracteres, finalizado pelo caractere nulo ( ' \0 ' ). O comprimento de uma string é, portanto, o número de caracteres antes do caractere nulo.

Aqui está um exemplo de como encontrar o comprimento de uma string usando a função `strlen`:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Olá Mundo!";
  int len = strlen(str);
  printf("Comprimento da string: %d\n", len);

  return 0;
}

```

**Saída:**

```
Comprimento da string: 10
```

É importante ressaltar que a função `strlen` não considera o caractere nulo na contagem. Portanto, no exemplo acima, o comprimento real da string "Olá Mundo!" é de 11 caracteres, mas `strlen` retorna 10.

Outra forma de obter o comprimento de uma string é usando um loop até encontrar o caractere nulo, como no exemplo abaixo:

```C
#include <stdio.h>

int main() {
  char str[] = "Hello World!";
  int len = 0;

  while (str[len] != '\0') {
    len++;
  }

  printf("Comprimento da string: %d\n", len);

  return 0;
}

```

**Saída:**

```
Comprimento da string: 12
```

## Deep Dive

Para aqueles que se perguntam como a função `strlen` funciona internamente, aqui está uma breve explicação. A função percorre cada caractere da string até encontrar o caractere nulo, contando o número de iterações necessárias para chegar a ele.

A função `strlen` é definida na biblioteca `string.h`, mas podemos criar nossa própria versão. Aqui está uma implementação simples que é equivalente à função `strlen`:

```C
int strlen(char *str) {
  int len = 0;

  while (str[len] != '\0') {
    len++;
  }

  return len;
}
```

Além disso, é importante lembrar que a função `strlen` é apenas uma das muitas funções úteis da biblioteca `string.h`. Vale a pena explorar e conhecer as demais para facilitar seu trabalho com strings em C.

## Veja também

- [Documentação oficial da função `strlen`](https://www.cplusplus.com/reference/cstring/strlen/)
- [Outras funções úteis da biblioteca `string.h`](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Tutorial sobre arrays e strings em C](https://www.geeksforgeeks.org/arrays-and-strings-in-c-2/)
- [Outras dicas sobre programação em C](https://www.codementor.io/blog/top-tips-for-writing-c-cpp-code-base-os2ojyt8r)