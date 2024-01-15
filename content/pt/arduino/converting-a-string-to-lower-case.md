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

## Por que converter uma string para minúsculas?

Quando trabalhamos com strings (cadeias de caracteres) em nossos programas Arduino, pode ser útil ter um método para converter todas as letras para minúsculas. Isso facilita a comparação de strings, já que letras maiúsculas e minúsculas são consideradas diferentes. Além disso, pode ser necessário converter uma string em minúsculas para garantir que ela esteja em um formato específico antes de ser enviada para outro dispositivo ou serviço.

## Como fazer:

Para converter uma string para minúsculas em um programa Arduino, podemos usar a função `toLowerCase()` da classe `String`. Veja abaixo um exemplo de como isso pode ser feito:

```Arduino
// Definimos uma string de exemplo
String palavra = "ARDUINO Rocks!";

// Convertemos a string para minúsculas usando toLowerCase()
palavra.toLowerCase();

// Imprimimos a string convertida
Serial.println(palavra);

// Saída: arduino rocks!
```

Note que a função `toLowerCase()` não altera a string original, mas retorna uma cópia convertida. Portanto, é importante atribuir o resultado da função a uma variável ou usar diretamente em uma operação.

## Mergulho Profundo:

A função `toLowerCase()` funciona convertendo cada caractere da string individualmente utilizando a tabela ASCII. Portanto, letras maiúsculas, que têm valores numéricos diferentes das minúsculas, são facilmente convertidas para suas versões minúsculas correspondentes. No entanto, outras variações de letras, como acentos ou caracteres especiais, podem não ser convertidas corretamente pela função. Nesse caso, pode ser necessário usar uma estratégia diferente, como criar um array de mapeamento com as conversões necessárias.

## Veja também:

- [Documentação da função toLowerCase()](https://www.arduino.cc/reference/pt/language/variables/data-types/stringobject/tolowercase/)
- [Página da tabela ASCII](https://pt.wikipedia.org/wiki/ASCII)