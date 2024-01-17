---
title:                "Convertendo uma string para maiúsculas"
html_title:           "Arduino: Convertendo uma string para maiúsculas"
simple_title:         "Convertendo uma string para maiúsculas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Capitalizar uma string é simplesmente deixar todas as letras maiúsculas em uma sequência de caracteres. Isso é algo que os programadores fazem para tornar o texto mais legível e padronizado.

## Como fazer:

Para capitalizar uma string usando o Arduino, podemos usar a função "toUpperCase()" que está disponível para a classe String. Veja um exemplo de código abaixo:

````Arduino
String texto = "exemplo de string";
texto.toUpperCase();
Serial.println(texto);
````

O resultado desse código seria:

````Arduino
EXEMPLO DE STRING
````

## Mergulho Profundo:

A prática de capitalizar strings é um padrão amplamente aceito em programação. Ela vem do tempo das máquinas de escrever onde as letras minúsculas eram mais difíceis de serem digitadas. Além disso, padronizar a capitalização torna o código mais fácil de ser lido e entendido por outros programadores.

Alternativamente, podemos usar a função "toUpper()" da biblioteca "ctype.h" para capitalizar uma string em C. No entanto, essa função funciona somente com caracteres individuais e não com strings inteiras.

Uma coisa importante a se notar é que essa função "toUpperCase()" não altera a string original, ela apenas retorna uma nova string. Portanto, é importante atribuir esse retorno a uma variável para utilizá-la posteriormente.

## Veja também:

- [Referência da função toUpperCase() no site do Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [Tutorial sobre como trabalhar com strings no Arduino](https://www.arduino.cc/en/Tutorial/StringConcatenation)
- [Explicação detalhada sobre a biblioteca ctype.h em C](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)