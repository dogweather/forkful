---
title:                "Extraindo subtrings"
html_title:           "Arduino: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Extrair substrings é a ação de separar uma parte específica de uma string maior. Isso é comumente feito por programadores para acessar informações específicas em uma string, como um código de identificação ou uma data. É uma forma eficiente de manipular e trabalhar com dados em um programa.

## Como fazer:

```Arduino
String myString = "Ola, mundo!"; // definir uma string
String substring = myString.substring(0, 3); // extrair a substring "Ola"

Serial.println(substring); // saída: "Ola"
```

Outro exemplo:

```Arduino
String sentence = "O numero pi é 3.14";
String pi = sentence.substring(12, 16);

Serial.println(pi); // saída: "3.14"
```

## Mergulho Profundo:

Extrair substrings tem suas raízes em linguagens de programação mais antigas, como C e BASIC. Em vez de usar a função substring, essas linguagens usavam operadores como ```substr``` em C e ```MID``` em BASIC. Hoje em dia, a função substring é amplamente usada em muitas linguagens de programação, incluindo Arduino.

Existem algumas alternativas para a função substring, como a função ```split```, que divide uma string em um array de substrings com base em um caractere específico. No entanto, a função substring ainda é preferida por muitos programadores por sua simplicidade e eficiência.

A implementação da função substring em Arduino é baseada na biblioteca String padrão, que oferece uma variedade de funções para trabalhar com strings. A função substring é definida como ```String.substring(index, length)```, onde index é a posição inicial da substring e length é o comprimento da substring.

## Veja também:

- [Documentação oficial da função substring em Arduino](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/substring/)
- [Tutorial sobre como usar a função substring em Arduino](https://www.arduino.cc/en/Tutorial/StringSubstring)
- [Fonte de informações sobre strings em Arduino](https://maker.pro/arduino/tutorial/arduino-string-functions-split-find-indexof-concatenate)