---
title:                "Capitalizando uma string"
html_title:           "Arduino: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Capitalizar uma string pode ser útil em várias situações, como, por exemplo, tornar o texto mais legível para o usuário ou para realizar comparações mais precisas. É uma técnica simples que pode facilitar significativamente o manuseio de strings em seu código.

## Como Fazer

Para capitalizar uma string em Arduino, podemos usar a função `toUpperCase()` do objeto String. Veja um exemplo abaixo:

```Arduino
// Declarando uma string
String texto = "ola mundo";

// Convertendo para maiúsculo
texto.toUpperCase();

// Imprimindo o resultado
Serial.println(texto); // OLA MUNDO
```

É importante lembrar que a função `toUpperCase()` não altera a string original, apenas retorna uma versão maiúscula dela. Portanto, para salvar a string modificada, você deve atribuí-la a uma nova variável ou à mesma variável original.

## Mergulho Profundo

Ao usar a função `toUpperCase()`, é importante ter em mente que ela irá converter todos os caracteres da string para maiúsculo, incluindo, por exemplo, letras com acentos ou caracteres especiais. Além disso, a conversão é feita de acordo com a tabela ASCII, o que significa que letras acentuadas podem ser convertidas para caracteres diferentes do esperado em alguns casos.

Uma alternativa seria criar sua própria função para capitalizar uma string, levando em consideração suas especificidades e necessidades. Outro detalhe importante é garantir que a string esteja sendo devidamente formatada antes de ser capitalizada, a fim de evitar erros inesperados no código.

## Veja Também

- [Tutorial sobre Strings no Arduino](https://www.arduino.cc/en/Tutorial/String)
- [Documentação oficial da função `toUpperCase()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [Tabela ASCII](https://www.ascii-code.com/)