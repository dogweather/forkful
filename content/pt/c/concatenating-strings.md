---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que e Porquê?
Concatenação de strings refere-se ao processo de combinarmos duas ou mais strings em uma única string. Fazemo-lo geralmente para criar frases dinâmicas e, às vezes, para economizar memória.

## Como Fazê-lo:
Aqui está um exemplo simples de como concatenar strings no C.

```C
#include <stdio.h>
#include <string.h>

int main() {
   char str1[20] = "Olá, ";
   char str2[] = "Mundo!";

   strcat(str1, str2);

   printf("%s\n", str1);  
   return 0;
}
```

O output deste código será:

```C
Olá, Mundo!
```

## Mergulho Profundo
1. *Contexto histórico:* A concatenação de strings surgiu desde os primeiros dias da programação. Era uma forma simples e direta de combinar dados de texto.

2. *Alternativas:* Em vez de `strcat()`, também podemos usar `strncat()` para especificar o número máximo de caracteres a serem concatenados.

    ```C
    strncat(str1, str2, 3);
    ```

3. *Detalhes de implementação:* `strcat()` e `strncat()` ambas alteram a string original, portanto, verifique por espaço suficiente na string de destino para evitar erros de execução.

## Veja Também
2. [Documentação strncat na Library GNU](https://www.gnu.org/software/libc/manual/html_node/Concatenating-Strings.html)