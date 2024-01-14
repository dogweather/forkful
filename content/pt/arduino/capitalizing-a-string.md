---
title:                "Arduino: Capitalizar uma string"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que capitalizar uma string em Arduino?

A programação em Arduino envolve a manipulação e processamento de strings, que são sequências de caracteres. Em alguns casos, pode ser necessário converter uma string para letras maiúsculas, seja para fins de formatação ou para fazer comparações. O processo de capitalização de uma string pode ser útil em diversas situações na programação.

## Como capitalizar uma string em Arduino?

Para capitalizar uma string em Arduino, primeiro é necessário converter a string para um array de caracteres utilizando a função `toCharArray()`. Em seguida, é possível utilizar um laço de repetição para percorrer cada caractere do array e converter para letras maiúsculas utilizando a função `toUpperCase()`. Ao final, a string convertida pode ser impressa ou utilizada para outros fins.

```Arduino
// Exemplo de capitalização de string em Arduino

String texto = "capitalizar";
char array[texto.length() + 1];

texto.toCharArray(array, texto.length() + 1); // converte a string para um array de caracteres

for (int i = 0; i < texto.length(); i++) {
  array[i] = toupper(array[i]); // converte cada caractere para letras maiúsculas
}

Serial.println(array); // imprime a string capitalizada "CAPITALIZAR"
```

## Aprofundando na capitalização de string

Existem diferentes formas de capitalizar uma string em Arduino, seja utilizando funções nativas da linguagem ou criando um código personalizado. Além disso, a capitalização pode afetar a performance do programa, por isso é importante avaliar qual é o método mais adequado para cada situação. Também é possível aplicar a capitalização não apenas para letras maiúsculas, mas também para letras minúsculas ou outros tipos de formatação.

Além disso, vale mencionar que a utilização de bibliotecas pode facilitar o processo de capitalização de string em Arduino. Existem várias opções disponíveis que podem oferecer funcionalidades extras e garantir uma maior eficiência no código.

## Veja também

- Documentação oficial do Arduino sobre strings: https://www.arduino.cc/reference/pt/language/variables/data-types/stringobject/
- Tutorial sobre como capitalizar string em Arduino: https://randomnerdtutorials.com/arduino-string-casestudy-number2/
- Biblioteca StringCase para formatação de strings em Arduino: https://github.com/bblanchon/ArduinoString/blob/master/doc/StringCase.md