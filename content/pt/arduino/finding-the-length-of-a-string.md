---
title:    "Arduino: Encontrando o comprimento de uma string"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com programação, muitas vezes precisamos manipular strings de texto. Uma operação muito comum é encontrar o comprimento de uma string, que pode ser útil para diversas aplicações. Neste post, vamos explorar como encontrar o comprimento de uma string usando o Arduino.

## Como fazer

Para começar, vamos criar uma variável do tipo `String` e atribuir um valor a ela. Podemos utilizar qualquer string, desde que esteja dentro das aspas.

```
Arduino String nome = "John Doe";
```

Agora, para encontrar o comprimento desta string, vamos usar a função `length()`. Esta função retorna o número de caracteres presentes na string.

```
Arduino int comprimento = nome.length();
```

Para verificar o resultado, podemos imprimir o valor da variável `comprimento` no monitor serial.

```
Arduino Serial.println(comprimento);
```

O programa completo ficará assim:

```
Arduino String nome = "John Doe";
Arduino int comprimento = nome.length();
Arduino Serial.println(comprimento);
```

Ao executar este código, o monitor serial irá mostrar o valor 8, pois a string tem 8 caracteres.

## Estudo aprofundado

Além da função `length()`, existem outras maneiras de encontrar o comprimento de uma string no Arduino. Uma delas é utilizar uma estrutura de repetição para percorrer a string e contar o número de caracteres, como no exemplo abaixo:

```
Arduino String nome = "John Doe";
Arduino int contador = 0;

// Loop para percorrer a string
for (int i = 0; i < nome.length(); i++) {
  contador++;
}

Arduino Serial.println(contador);
```

Outra opção é utilizar a biblioteca `strlen.h`, que já possui uma função específica para encontrar o comprimento de uma string.

```
#include <strlen.h>

Arduino String nome = "John Doe";
Arduino int comprimento = strlen(nome);

Arduino Serial.println(comprimento);
```

Independentemente do método escolhido, o resultado será o mesmo: o comprimento da string.

## Veja também

- [Documentação oficial do Arduino sobre a função length()](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/length/)
- [Tutorial sobre como trabalhar com strings no Arduino](https://www.hackster.io/SiliconFotociencia/manipulando-strings-no-arduino-313624)

Esperamos que este post tenha sido útil para aprender como encontrar o comprimento de uma string no Arduino. Agora que você já sabe como fazer, pode aplicar esse conhecimento em seus projetos. Divirta-se programando!