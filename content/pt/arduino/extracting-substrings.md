---
title:    "Arduino: Extraindo substrings"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que usar subcadeias de caracteres em programação Arduino?

A extração de subcadeias de caracteres é útil quando você precisa manipular e trabalhar com apenas uma parte de uma string maior. Isso pode tornar sua programação mais eficiente e organizada.

## Como fazer a extração de subcadeias de caracteres no Arduino

No Arduino, podemos usar a função `substring()` para extrair pedaços de uma string maior. Veja um exemplo de como usá-la:

```Arduino
String texto = "Olá mundo!";
String palavra = texto.substring(0, 3); // extrai a primeira palavra do texto

Serial.println(palavra); // imprime "Olá"
```

Neste exemplo, usamos a função `substring()` para extrair a primeira palavra da string "Olá mundo!", que é "Olá". A função recebe dois parâmetros: o primeiro é o índice inicial da substring que queremos extrair, e o segundo é o índice final, que é opcional, e indica onde deve parar a extração. Se não for informado, a extração vai até o final da string original.

Outra função útil para extrair subcadeias de caracteres é a `indexOf()`. Ela retorna o índice da primeira ocorrência de um caractere ou string dentro de uma string maior. Veja um exemplo:

```Arduino
String fruta = "banana";
int indice = fruta.indexOf("na"); // procura por "na" na string e retorna o índice da primeira ocorrência

Serial.println(indice); // imprime 2
```

Também é possível usar a função `charAt()` para retornar o caractere em uma posição específica da string. Veja um exemplo:

```Arduino
String texto = "Olá mundo!";
char letra = texto.charAt(4); // retorna o caractere na posição 4 da string, que é " "

Serial.println(letra); // imprime " "
```

## Aprofundando na extração de subcadeias de caracteres

Além das funções mencionadas, existem outras formas de extrair subcadeias de caracteres no Arduino. Uma delas é usando ponteiros para manipular a string original. Isso pode ser útil quando você lida com strings muito grandes e quer economizar memória.

Outro ponto importante é ter cuidado com a manipulação das strings, pois se não forem feitas corretamente, podem causar erros no código ou até mesmo travar o Arduino.

## Veja também
- Documentação da função `substring()`: https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/substring/
- Tutorial sobre manipulação de strings no Arduino: https://www.filipeflop.com/blog/manipulacao-de-strings-no-arduino/
- Vídeo explicando como extrair subcadeias de caracteres no Arduino: https://www.youtube.com/watch?v=vg-lDhYY5yc