---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Concatenar strings é o processo de unir duas ou mais strings em uma. Os programadores fazem isso para manipular e gerenciar dados de texto de forma eficiente.

## Como Fazer:

Veja um exemplo de como concatenar strings no Arduino:

```Arduino
String string1 = "Olá, ";
String string2 = "Portugal!";
String string3 = string1 + string2; // Concatenação
Serial.print(string3); // Imprime "Olá, Portugal!"
```

## Análise Mais Aprofundada

Historicamente, a concatenação de strings é um aspecto central da manipulação de dados, usada desde os primeiros dias da programação. No contexto do Arduino, a concatenação de strings se tornou ainda mais crucial com a implementação do tipo de dados String. 

Existem alternativas para concatenar strings, como usar a função `strcat` de 'string.h', mas a maioria dos programadores prefere o operador `+` pelo seu conforto e simplicidade.

A implementação da concatenação no Arduino é feita através da sobrecarga do operador `+` para o objeto String, permitindo a união simples de strings reorganizando a memória necessária.

## Veja Também

1. Manual Arduino para `String` library: [https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
2. Guia Arduino String Manipulation: [https://startingelectronics.org/articles/arduino/switch-case-string/](https://startingelectronics.org/articles/arduino/switch-case-string/)