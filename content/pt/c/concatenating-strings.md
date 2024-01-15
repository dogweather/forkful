---
title:                "Concatenando strings"
html_title:           "C: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Existem muitas situações em que precisamos combinar várias strings em um único texto. Isso pode ser útil ao criar mensagens personalizadas, gerar relatórios dinâmicos ou construir URLs. A concatenação de strings nos permite unir diferentes partes de um texto em uma única variável para uso posterior.

## Como fazer

```C
#include <stdio.h>
#include <string.h>

int main() {

    // Definindo duas strings
    char nome[] = "João";
    char sobrenome[] = "Silva";

    //Concatenando as strings
    strcat(nome, sobrenome);

    // Imprimindo o resultado
    printf("%s\n", nome);

    return 0;
}
```

Output: João Silva

No exemplo acima, usamos a função `strcat()` da biblioteca `string.h` para concatenar as strings `nome` e `sobrenome` em uma única variável. É importante lembrar que a string de destino (no caso, `nome`) deve ter espaço suficiente para acomodar a string concatenada.

## Deep Dive

Além da função `strcat()`, também existem outras maneiras de concatenar strings em C. Por exemplo, podemos usar a função `sprintf()` para combinar strings com valores numéricos ou até mesmo utilizar operadores de atribuição, como o `+=`, para adicionar strings em variáveis já existentes.

No entanto, é preciso ter cuidado ao manipular strings em C, pois a linguagem não possui um tipo de dado específico para esse fim. Isso significa que não podemos simplesmente somar ou subtrair strings como se fossem números, algumas funções específicas como `strcat()` devem ser usadas para garantir que a concatenação seja feita corretamente.

## Veja também

- [Tutorial sobre manipulação de strings em C](https://www.learn-c.org/en/Strings)
- [Documentação oficial sobre a biblioteca string.h](https://www.tutorialspoint.com/c_standard_library/string_h.htm)