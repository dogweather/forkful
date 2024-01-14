---
title:                "Arduino: Encontrando o comprimento de uma sequência"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string é importante?

Encontrar o comprimento de uma string é uma habilidade valiosa para programadores de Arduino. Isso permite que você manipule e processe dados de entrada de forma eficiente, garantindo que seu código funcione corretamente. Além disso, muitas vezes você precisará de informações sobre o tamanho de uma string ao trabalhar com dispositivos externos ou comunicações serial.

## Como fazer:

Para encontrar o comprimento de uma string em Arduino, é necessário utilizar a função `strlen()` da biblioteca `string.h`. Esta função retorna o número de caracteres em uma string, incluindo o caractere nulo.

```
Arduino #include <string.h>

void setup() {
  String minhaString = "Olá, mundo!";
  int comprimento = strlen(minhaString);

  Serial.begin(9600);
  Serial.println(comprimento); // Saída: 12
}

void loop() {
  // Seu código aqui
}
```

## Aprofundando:

Ao utilizar a função `strlen()`, é importante lembrar que ela retorna o número de caracteres e não a posição do último caractere. Além disso, não é possível encontrar o tamanho de um array de caracteres com `strlen()`. Nesse caso, é necessário utilizar `sizeof()`.

```
Arduino #include <string.h>

void setup() {
  char meuArray[10] = "Olá"; // Tamanho do array é de 10 bytes
  int tamanho = sizeof(meuArray);

  Serial.begin(9600);
  Serial.println(tamanho); // Saída: 10
}

void loop() {
  // Seu código aqui
}
```

## Veja também:

Abaixo estão alguns links úteis para aprender mais sobre a função `strlen()` e como encontrar o comprimento de uma string em Arduino.

- [Documentação oficial da função strlen()](https://www.arduino.cc/reference/en/language/functions/strings/strlen/)
- [Tutorial completo sobre strings em Arduino](https://www.arduino.cc/en/Tutorial/StringLengthTrim)
- [Explicação sobre a diferença entre strlen() e sizeof()](https://www.cplusplus.com/reference/cstring/strlen/)