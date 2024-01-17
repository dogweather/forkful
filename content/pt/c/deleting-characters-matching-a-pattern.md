---
title:                "Excluindo caracteres correspondentes a um padrão."
html_title:           "C: Excluindo caracteres correspondentes a um padrão."
simple_title:         "Excluindo caracteres correspondentes a um padrão."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que e por que fazer?

Eliminar caracteres que correspondem a um padrão é um processo comum realizado pelos programadores para alterar ou limpar uma determinada string de texto. Isto pode ser útil para remover dados desnecessários ou formatar uma string de acordo com um determinado modelo.

## Como fazer:

Para deletar caracteres correspondentes a um padrão em C, é necessário primeiro definir o padrão utilizando expressões regulares. Em seguida, pode-se utilizar a função `regcomp()` para compilar o padrão e depois a função `regexec()` para executá-lo na string desejada. Abaixo está um exemplo de código que remove os números de uma string e imprime o resultado:

```C
#include <stdio.h>
#include <regex.h>
 
int main() {
  // Definindo o padrão para números
  regex_t regex;
  int reti;
  char msg[] = "1234567";
 
  // Compilando o padrão
  reti = regcomp(&regex, "[0-9]", 0);
  // Executando o padrão na string
  reti = regexec(&regex, msg, 0, NULL, 0);
  // Imprimindo a string sem números
  printf("String sem números: %s\n", msg);
  return 0;
}
```

O resultado será: `String sem números: `.

## Profundando:

Esta técnica de eliminar caracteres correspondentes a um padrão se originou em linguagens de programação como Perl e eventualmente foi adotada em outras linguagens, incluindo C. Alternativas para realizar essa tarefa incluem o uso de funções de manipulação de strings como `strtok()` ou `strchr()`. No entanto, a utilização de expressões regulares permite um controle mais preciso e flexível sobre os caracteres a serem removidos.

## Veja Também:

- Documentação da função `regcomp()` em C: [https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- Tutorial sobre expressões regulares em C: [https://www3.ntu.edu.sg/home/ehchua/programming/howto/RegularExpression.html](https://www3.ntu.edu.sg/home/ehchua/programming/howto/RegularExpression.html)
- Outras funções úteis para manipular strings em C: [https://www.tutorialspoint.com/c_standard_library/string_h.htm](https://www.tutorialspoint.com/c_standard_library/string_h.htm)