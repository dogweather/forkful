---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Determinar o comprimento de uma string é avaliar quantos caracteres ela tem. Nós, programadores, fazemos isso para manipular e gerenciar efetivamente os dados de texto, seja para validação de entrada ou para transformação de dados.

## Como fazer:

Aqui está um código Arduino para determinar o comprimento de uma string.

```Arduino
String minhaString = "Programação Arduino";
int comprimento = minhaString.length();

Serial.begin(9600);
Serial.println(comprimento);
```
A saída será '21', que é o número de caracteres na string "Programação Arduino".

## Aprofundando

1. **Contexto Histórico**: A função length foi introduzida nos primeiros dias do C e C++ e, posteriormente, foi incorporada na linguagem Arduino, que é basicamente C/C++ com bibliotecas extras.
2. **Alternativas**: Há outras maneiras de encontrar o comprimento de uma string como usar um loop para percorrer cada caracter até encontrar o terminador nulo. No entanto, a função length() é mais prática e menos suscetível a erros.
3. **Detalhes de Implementação**: A função length() retorna o número de caracteres na string, não incluindo o caractere nulo de terminação ('\0').

## Veja também

- [Referência oficial da função length() no Arduino](https://www.arduino.cc/en/Tutorial/StringLengthTrim)
- [Arduino String Manipulation Using Minimal Ram](https://forum.arduino.cc/index.php?topic=131806.0)