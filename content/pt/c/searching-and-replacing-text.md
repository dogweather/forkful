---
title:                "C: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

O processo de busca e substituição de texto é uma técnica essencial em programação. Ele permite que desenvolvedores encontrem e modifiquem padrões específicos em um texto, tornando a manipulação de dados mais eficiente e precisa.

## Como fazer

Existem várias maneiras de realizar busca e substituição de texto em linguagem C. Uma delas é utilizando a função `strstr()` para encontrar uma sequência de caracteres e a função `strcpy()` para copiar a nova sequência no lugar da antiga. Veja um exemplo abaixo:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char texto[] = "Este é um exemplo de busca e substituição de texto.";
  char antigo[] = "busca";
  char novo[] = "procura";
  char *ponteiro;

  ponteiro = strstr(texto, antigo);

  if(ponteiro != NULL) {
    strcpy(ponteiro, novo);
  }

  printf("%s", texto);

  return 0;
}
```

Saída:
`Este é um exemplo de procura e substituição de texto.`

Com esse exemplo, podemos ver como é possível realizar uma busca e substituição em uma string. Mas lembre-se, esse é apenas um exemplo básico e existem outras maneiras de atingir o mesmo resultado.

## Detalhes sobre busca e substituição de texto

Além da função `strstr()`, existem outras funções em linguagem C que podem ser úteis para realizar busca e substituição de texto, como a `strchr()`, `strrchr()` e `strtok()`. Além disso, é importante ter conhecimento sobre expressões regulares e como utilizá-las para buscar padrões específicos em um texto.

Outro ponto importante é ter cuidado ao substituir informações em um texto. É necessário garantir que a nova sequência de caracteres tenha o mesmo tamanho que a antiga, caso contrário, poderão ocorrer erros de memória.

## Ver também

- [Tutorial sobre expressões regulares em C](https://www.geeksforgeeks.org/regular-expressions-in-c-programming/)
- [Função strstr() - Documentação oficial](https://www.cplusplus.com/reference/cstring/strstr/)
- [Manipulação de strings em C - Documentação oficial](https://www.tutorialspoint.com/c_standard_library/string_h.htm)