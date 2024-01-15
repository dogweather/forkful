---
title:                "Utilizando expressões regulares"
html_title:           "C: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em C?

Expressões regulares são uma ferramenta poderosa que permitem procurar, substituir e validar padrões em texto. Em C, elas podem ser usadas para manipular strings com maior facilidade e eficiência. Se você está trabalhando com texto em seus programas, aprender a usar expressões regulares pode economizar tempo e tornar seu código mais robusto.

## Como usar expressões regulares em C

Para usar expressões regulares em C, primeiro é necessário incluir a biblioteca de expressões regulares `regex.h` no seu programa. Em seguida, você pode criar um objeto do tipo `regex_t` para armazenar a expressão regular que você deseja usar. Para compilar a expressão regular, você pode usar a função `regcomp()` e, em seguida, usar a função `regexec()` para procurar padrões em uma string. Por exemplo:

```C
#include <stdio.h>
#include <regex.h>

int main() {
  regex_t regex;
  char *text = "Olá, este é um texto de exemplo.";
  char *pattern = "exemplo";
  int result = regcomp(&regex, pattern, 0);
  
  if (result == 0) {
    result = regexec(&regex, text, 0, NULL, 0);
    
    if (result == 0) {
      printf("Padrão encontrado na string!\n");
    } else {
      printf("Padrão não encontrado na string.\n");
    }
    
    regfree(&regex);
  } else {
    printf("Erro ao compilar a expressão regular.\n");
  }
  
  return 0;
}
```

O código acima utiliza a função `regcomp()` para compilar o padrão "exemplo" e a função `regexec()` para verificar se esse padrão existe na string fornecida. A função `regfree()` é usada para liberar a memória alocada para o objeto `regex`.

## Profundando em expressões regulares em C

As expressões regulares em C são baseadas na biblioteca POSIX regex, que segue um conjunto de regras e metacaracteres específicos. Alguns exemplos de metacaracteres são `.` para qualquer caractere, `*` para multiplicidade zero ou mais e `+` para multiplicidade uma ou mais. Existem também modos de busca que permitem procurar por padrões em todo o texto ou apenas no início ou fim do texto. Para aprender mais sobre a sintaxe e recursos de expressões regulares em C, consulte a documentação oficial da biblioteca `regex.h`.

## Veja também

- [Documentação da biblioteca regex.h](https://www.gnu.org/software/libc/manual/html_node/POSIX-Regexps.html)
- [Tutorial sobre expressões regulares em C](https://www.thegeekstuff.com/2017/02/c-regex/)
- [Códigos de exemplo com expressões regulares em C](https://github.com/zakj/regex-example-c)