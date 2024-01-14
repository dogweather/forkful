---
title:                "Arduino: Convertendo uma string para letras minúsculas"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas é importante

Ao trabalhar com dados em um projeto Arduino, muitas vezes é necessário manipular strings. Converter uma string para letras minúsculas pode ser útil ao realizar comparações de texto, pois algumas funções em C++ são sensíveis a maiúsculas e minúsculas. Além disso, pode ser necessário formatar a saída de dados em letras minúsculas para uma melhor legibilidade.

## Como converter uma string para letras minúsculas no Arduino

Para converter uma string para letras minúsculas no Arduino, é necessário utilizar a função `toLowerCase()` da biblioteca `String`. Esta função percorre a string original e substitui todas as letras maiúsculas por letras minúsculas.

```
Arduino#include <String.h>

String texto = "Olá, Mundo!";
texto.toLowerCase(); // "olá, mundo!"

Serial.println(texto); // saída: "olá, mundo!"
```

## Aprofundando na conversão de strings para letras minúsculas

É importante ressaltar que a função `toLowerCase()` não é capaz de converter caracteres especiais ou acentos em letras minúsculas. Além disso, ela é sensível ao idioma do sistema em que o código está sendo executado, ou seja, em um sistema que utiliza o alfabeto latino, a função converterá `A` para `a`, enquanto em um sistema que utiliza o alfabeto cirílico, ela converterá `А` para `а`.

Caso seja necessário converter caracteres especiais ou acentos em letras minúsculas, é possível utilizar funções adicionais ou uma biblioteca terceirizada. Além disso, é importante ter em mente que a biblioteca `String` pode ocupar bastante memória do microcontrolador, por isso, é recomendado utilizar outros tipos de variáveis, como `char`, para manipular strings.

## Veja também

- [Função `toLowerCase()` na documentação oficial do Arduino](https://www.arduino.cc/reference/pt/language/variables/data-types/stringobject/)
- [Biblioteca para conversão de acentos e caracteres especiais em letras minúsculas](https://www.arduino.cc/reference/pt/language/variables/data-types/stringobject/)
- [Exemplo de conversão de string para letras minúsculas em um projeto Arduino](https://www.arduino.cc/en/Tutorial/BuiltInExamples/Tour/StringCaseChanges/)