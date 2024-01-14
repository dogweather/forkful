---
title:    "Arduino: Encontrando o comprimento de uma string."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Ao programar em Arduino, muitas vezes temos a necessidade de manipular strings, ou seja, sequências de caracteres. Saber a quantidade de caracteres de uma string pode ser muito útil em diversas situações, como por exemplo, para validar entradas de dados ou para determinar o tamanho de um array. Neste artigo, ensinaremos como encontrar o comprimento de uma string utilizando Arduino.

## Como fazer?

Existem algumas maneiras diferentes de encontrar o comprimento de uma string em Arduino. A mais simples é usando a função `strlen()`, que está presente na biblioteca `string.h`. Veja um exemplo de código abaixo:

```
Arduino void setup() {
  Serial.begin(9600);
  String minhaString = "Hello World";
  int tamanho = strlen(minhaString);
  Serial.println(tamanho);
}
```

O código acima irá imprimir `11`, que é o número de caracteres da string "Hello World". Você também pode usar o método `length()` de um objeto String, que retorna o comprimento da string. Veja abaixo outro exemplo de código:

```
Arduino void loop() {
  String minhaString = "Arduino";
  Serial.println(minhaString.length());
  delay(500);
}
```

Este código irá imprimir `7`, que é o comprimento da string "Arduino".

## Mergulho Profundo

Além da função `strlen()` e do método `length()`, é possível encontrar o comprimento de uma string utilizando estruturas de repetição, como o `for`. Veja um exemplo abaixo:

```
Arduino void setup() {
  Serial.begin(9600);
  char minhaString[] = "Um exemplo de string";
  int tamanho = 0;
  for (int i = 0; i < 19; i++) {
    tamanho++; // incrementa o tamanho a cada iteração
  }
  Serial.println(tamanho);
}
```

Neste exemplo, utilizamos um array de caracteres e um `for` para percorrer cada elemento do array e incrementar o tamanho a cada iteração. O resultado impresso será `19`, que é o número de caracteres da string.

Outra opção é usar o método `getCapacity()`, que retorna a capacidade máxima de caracteres de um objeto String. No entanto, este método calcula o tamanho total da memória alocada para a string, incluindo espaços em branco e caracteres nulos, e pode não ser o valor esperado em alguns casos.

## Veja também

- Documentação oficial (em inglês) para a função `strlen()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/
- Documentação oficial (em inglês) para o método `length()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/
- Tutorial (em inglês) para encontrar o comprimento de uma string em Arduino: https://howtomechatronics.com/tutorials/arduino/how-to-find-the-length-of-a-string-in-arduino/