---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

A interpolação de string é a inclusão de variáveis diretamente em strings. Os programadores fazem isso para tornar o código mais legível e eficiente, em vez de usar concatenação de string.

## Como Fazer:

Um exemplo prático é demonstrado abaixo. Para a interpolação de string, nós usamos a função `sprintf` no C. Observe:

```C
#include <stdio.h>

int main() {
    char buffer[50];
    int a = 10, b = 20;

    sprintf(buffer, "A soma de %d e %d é %d.", a, b, a + b);
    
    printf("%s\n", buffer);

    return 0;
}
```

A saída deste programa será:

```C
A soma de 10 e 20 é 30.
```

## Aprofundando o Assunto

Historicamente, a interpolação de strings não foi inicialmente suportada em C, o que levou à adoção da função `sprintf` para realizar essa tarefa. 

Há outras alternativas para interpolação de strings em C, como utilizar a função `snprintf`, que é semelhante ao `sprintf`, mas mais segura, pois inclui o tamanho do buffer como argumento para evitar a sobrecarga de buffer.

Em termos de implementação, `sprintf` basicamente leva uma string e argumentos separados, e formata a string preenchendo os marcadores de posição com os argumentos.

## Veja Também

- [Documentação sprintf - cplusplus.com](http://www.cplusplus.com/reference/cstdio/sprintf/)
- [Documentação snprintf - cplusplus.com](https://www.cplusplus.com/reference/cstdio/snprintf/)
- [Interpolação de String no C# - Microsoft Docs](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/interpolation)