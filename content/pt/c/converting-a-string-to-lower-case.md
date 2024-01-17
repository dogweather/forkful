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

## O que é e por que fazemos isso?
Converter uma string para minúsculas é um processo em que todas as letras maiúsculas em uma string são convertidas para letras minúsculas. Isso é feito principalmente para garantir que a entrada do usuário seja tratada de forma consistente e para facilitar a comparação entre strings.

## Como fazer:
```
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Ola, Mundo!";
  printf("String original: %s\n", str);

  // Converter para minúsculas
  for(int i = 0; str[i]; i++){
    if(str[i] >= 'A' && str[i] <= 'Z'){  // verifica se o caractere é maiúsculo
      str[i] = str[i] + 32;  // converte para minúsculo
    }
  }

  printf("String convertida: %s\n", str);

  return 0;
}
```

Saída:
String original: Ola, Mundo!
String convertida: ola, mundo!

## Mergulho profundo:
Concluir uma conversão de string para minúsculas não é tão simples quanto parece. Historicamente, dependia do conjunto de caracteres usado pelos sistemas operacionais. Hoje em dia, existem várias maneiras de fazer isso, incluindo funções de biblioteca como tolower() e funções específicas de plataforma. É importante escolher a abordagem correta, dependendo da sua necessidade específica.

## Veja também:
- https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm
- https://www.geeksforgeeks.org/converting-string-lower-upper-case-using-tolower-toupper/
- https://www.gnu.org/software/libc/manual/html_node/Locale-Selection.html#Locale-Selection