---
title:                "Convertendo uma string para minúsculas"
html_title:           "Arduino: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Converter uma string para letras minúsculas é um processo comum na programação que permite transformar todas as letras em uma string em minúsculas. Isso pode ser útil para comparação de strings ou para garantir consistência nos dados, independentemente de como eles foram originalmente digitados pelo usuário.

## Como fazer:

Existem várias maneiras de converter uma string para letras minúsculas no Arduino. Uma opção é utilizar a função `toLowerCase()` que está disponível na biblioteca `String`. Veja o exemplo abaixo:

```Arduino
#include <String.h> // inclui a biblioteca String

void setup() {
  Serial.begin(9600); // inicia a comunicação serial
  String frase = "OLA AMIGOS"; // cria uma string com todas as letras maiúsculas
  Serial.println(frase.toLowerCase()); // imprime a string convertida para minúsculas
}

void loop() {
  // vazio porque não é necessário no exemplo
}
```

A saída deste código será `ola amigos`, com todas as letras em minúsculas.

## Mergulho profundo:

O processo de converter strings para letras minúsculas tem sido utilizado há muitos anos na programação, como uma forma de padronizar os dados e facilitar a comparação de strings. Existem outras formas de realizar essa conversão, como por exemplo utilizando um loop para percorrer a string e alterar manualmente cada letra para minúscula.

No caso específico do Arduino, a função `toLowerCase()` é uma opção mais simples e eficiente para realizar a conversão. Entretanto, é importante lembrar que a biblioteca `String` pode ocupar bastante espaço na memória do microcontrolador, então é recomendável utilizar esse recurso com cuidado e somente quando necessário.

## Veja também:

- [Documentação oficial do Arduino sobre a função `toLowerCase()`](https://www.arduino.cc/reference/en/language/functions/string/character-case-conversion/tolowercase/)
- [Outras bibliotecas e funções úteis para manipulação de strings no Arduino](https://create.arduino.cc/projecthub/Arduino_Scuola/string-manipulation-functions-in-arduino-ede111)