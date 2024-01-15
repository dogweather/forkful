---
title:                "Convertendo uma string para minúsculas"
html_title:           "C: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que
Se você já teve a necessidade de usar strings em seu código C e se deparou com a diferença de capitalização entre elas, provavelmente já pensou em converter todas para o mesmo formato. Neste artigo, você vai aprender como fazer isso de forma simples e eficiente.

## Como Fazer
Para iniciar o processo de conversão de uma string para letras minúsculas, você precisa utilizar a função `tolower` da biblioteca padrão `ctype.h`. Essa função recebe como parâmetro um caractere e retorna o seu equivalente em minúsculo. Então, para converter uma string completa, basta percorrê-la letra por letra e aplicar a função.

Veja um exemplo de código abaixo:

```
#include <stdio.h>
#include <ctype.h>

int main() {
  char palavra[] = "ExemploDeString";
  int i;

  for(i = 0; palavra[i]; i++) {
    palavra[i] = tolower(palavra[i]);
  }

  printf("String em letras minúsculas: %s", palavra); 
  // saída: "exemplodestring"

  return 0;
}
```

A função `tolower` também pode ser utilizada em conjunto com outras funções de manipulação de strings, como `strlen` e `strcpy`, para realizar tarefas mais complexas. Além disso, é importante lembrar de sempre tratar caracteres especiais e acentos, para evitar possíveis erros na conversão.

## Deep Dive
Para uma conversão mais precisa, é interessante ter conhecimento sobre a tabela ASCII, pois é ela que define a ordem e representação dos caracteres em um computador. Por exemplo, a letra "A" está no valor 65 e a letra "a" está no valor 97. Com isso em mente, podemos entender melhor a função `tolower`, que retorna o valor correspondente da letra em minúsculo, de acordo com a tabela ASCII.

Além disso, é possível utilizar a função `toupper` para converter uma string para letras maiúsculas, seguindo o mesmo raciocínio.

## Veja Também
Para mais informações sobre strings e funções de manipulação de strings em C, confira os links abaixo:

- [Documentação oficial sobre a função tolower](https://www.cplusplus.com/reference/cctype/tolower/)
- [Explicação sobre a tabela ASCII](https://www.ascii-code.com/)
- [Tutorial sobre manipulação de strings em C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)