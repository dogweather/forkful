---
title:                "Interpolando uma string"
html_title:           "C: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que e Por que?
Interpolar uma string e simplesmente substituir variaveis em uma frase ou texto. Isso e util para criar strings dinamicas que mudam de acordo com as variaveis envolvidas. Programadores usam isso para tornar seus codigos mais flexiveis e dinamicos.

## Como fazer:
Aqui esta um exemplo simples de interpolacao de string em C:
```C
#include <stdio.h>

int main() {
  char *nome = "Joao";
  int idade = 25;
  printf("Meu nome e %s e tenho %d anos.", nome, idade);
  return 0;
}
```
Output:
```
Meu nome e Joao e tenho 25 anos.
```

## Profundidade:
A interpolacao de string eh comumente usada em linguagens de programacao, mas teve sua origem no COBOL na decada de 1950. Algumas alternativas para interpolacao de string incluem concatenacao manual de strings e a funcao sprintf em C. Para implementar a interpolacao de string em C, eh necessario usar a funcao printf e especificar os tipos de dados das variaveis envolvidas.

## Veja Tambem:
- [Documentacao oficial do C sobre a funcao printf](https://docs.microsoft.com/pt-br/cpp/c-runtime-library/reference/printf-printf-l-wprintf-wprintf-l?view=msvc-160)
- [Artigo sobre interpolacao de string em C](https://www.tutorialspoint.com/cprogramming/c_string_interpolation.htm)
- [Tutorial de C para iniciantes](https://www.geeksforgeeks.org/c-programming-language/)