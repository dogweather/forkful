---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Converter uma string em letras minúsculas é basicamente transformar todas as letras maiúsculas de uma string em suas versões minúsculas. Os programadores fazem isso para normalizar dados para comparação, armazenamento ou processamento.

## Como fazer:

Aqui está um exemplo de código sobre como você pode fazer isso no seu Arduino:

```Arduino
String minhaString = "OI, EU SOU O ARDUINO";
minhaString.toLowerCase();
Serial.println(minhaString); // imprimirá: "oi, eu sou o arduino"
```
Então, depois de chamar o método `toLowerCase()`, todas as letras da string `minhaString` se tornaram minúsculas.

## Mergulho profundo:

O método `toLowerCase()` está presente em muitas linguagens de programação além do Arduino/C++, é uma convenção derivada da linguagem de programação original C. Para alternativas, você poderia manualmente iterar por cada caractere na string e usar o método `tolower()` da biblioteca `ctype.h`.

Por trás dos panos, `toLowerCase()` funciona percorrendo cada caractere na string, e se for uma letra maiúscula, é substituída pelo equivalente em minúscula. Ele faz isso usando a tabela ASCII, que tem uma correspondência direta entre letras maiúsculas e minúsculas.

## Veja também:

Para mais detalhes, confira a documentação oficial do Arduino para a função `String::toLowerCase()` [aqui](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/).

Também dê uma olhada na função `tolower()` da biblioteca `ctype.h`, caso queira realizar esse procedimento manualmente. Aqui está a [documentação](https://www.cplusplus.com/reference/cctype/tolower/).