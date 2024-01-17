---
title:                "Encontrando o comprimento de uma string"
html_title:           "Arduino: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?
Encontrar o comprimento de uma string significa determinar a quantidade de caracteres que ela contém. Programadores muitas vezes precisam fazer isso para manipular e processar dados de texto em seus códigos.

## Como fazer:
Você pode encontrar o comprimento de uma string usando a função `length()`. Por exemplo, o seguinte código irá retornar o comprimento da string "Olá, mundo!":
```
ArduinoString str = "Olá, mundo!";
int length = str.length();
Serial.println(length); //imprime 12
```

## Aprofundamento:
A função `length()` foi adicionada ao Arduino na sua versão 0012 e está disponível para todas as Strings. Existem outras formas de encontrar o comprimento de uma string, como usar a função `strlen()` da biblioteca `string.h`. No entanto, essa função não está disponível para todas as plataformas suportadas pelo Arduino e também é menos eficiente em termos de uso de memória.

## Veja também:
- Documentação oficial do Arduino sobre a função `length()`: https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/length/
- Tutorial do SparkFun sobre Strings no Arduino: https://learn.sparkfun.com/tutorials/using-the-serial-flash-sequencer-with-python