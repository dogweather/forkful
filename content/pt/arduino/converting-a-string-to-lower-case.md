---
title:    "Arduino: Convertendo uma string para minúsculas"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que Convergir uma String para Minúsculas?

Convertendo uma string para letras minúsculas é um processo comum na programação, especialmente quando se trabalha com entradas de usuários ou manipulação de dados. Isso permite que os dados sejam padronizados e comparados facilmente. Além disso, ao converter para minúsculas, evitamos problemas de sensibilidade de case, tornando nossos programas mais robustos e precisos.

## Como Fazer

Para converter uma string para minúsculas em Arduino, primeiro precisamos armazenar a string em uma variável. Em seguida, usamos a função `toLowerCase()` para realizar a conversão. Por exemplo:

```arduino
// Definimos a string original
String minhaString = "ARDUINO";

// Usamos a função toLowerCase() para converter
minhaString.toLowerCase();

// Imprime a string convertida
Serial.println(minhaString);
```

O resultado no monitor serial será "arduino", com todas as letras em minúsculas.

## Mergulho Profundo

O processo de conversão de uma string em Arduino envolve a manipulação de cada caractere individualmente. Isso é feito usando a tabela ASCII, que associa números a caracteres. Ao subtrair 32 do valor ASCII de um caractere em maiúsculo, obtemos seu equivalente em minúsculo. No exemplo acima, a letra "A" tem o valor ASCII 65, enquanto "a" tem o valor 97. Ao subtrair 32 de 65, obtemos 33, que é o valor ASCII de "a".

Além disso, devemos ter cuidado com a sensibilidade de case em outras funções, como `charAt()` e `equalsIgnoreCase()`. Ao comparar strings, é importante garantir que todas estejam no mesmo caso ou usar a função `toLowerCase()` antes da comparação.

## Veja Também

- Tutorial: [Case Conversion in Arduino](https://www.arduino.cc/en/Tutorial/TextCaseConversion)
- Documentação Oficial: [`toLowerCase()`](https://www.arduino.cc/reference/pt/language/variables/data-types/string/functions/tolowercase/)