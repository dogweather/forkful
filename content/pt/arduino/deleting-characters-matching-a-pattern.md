---
title:                "Arduino: Deletando caracteres que correspondem a um padrão."
simple_title:         "Deletando caracteres que correspondem a um padrão."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com dados em uma string que não são relevantes para o seu projeto Arduino? Talvez você precise remover caracteres específicos de uma string antes de usá-la em um sensor ou display. Nesse caso, aprender a apagar caracteres que correspondem a um padrão é uma habilidade útil para qualquer programador de Arduino.

## Como Fazer

Você pode usar a função `removeMatchingCharacters()` da biblioteca Arduino String para remover caracteres que correspondem a um padrão em uma string. Aqui está um exemplo de código que remove todos os números de uma string:

```Arduino
#include <Arduino.h>
#include <String.h>

String originalString = "Olá123 mundo456";
originalString.removeMatchingCharacters("0123456789");
Serial.println(originalString);
```

O output desse código seria "Olá mundo", já que os números foram removidos da string original.

## Exploração em Profundidade

A função `removeMatchingCharacters()` também permite que você especifique o tipo de caractere que deseja remover usando um argumento opcional. Por exemplo, para remover todos os caracteres de pontuação de uma string, você pode usar o seguinte código:

```Arduino
originalString.removeMatchingCharacters("punct");
```

Além disso, você pode usar expressões regulares (Regular Expressions) na função `removeMatchingCharacters()` para especificar padrões mais complexos. Por exemplo, se você quiser remover todas as letras maiúsculas e minúsculas de uma string, você pode usar o seguinte código:

```Arduino
originalString.removeMatchingCharacters("[a-zA-Z]");
```

Explore a documentação da função `removeMatchingCharacters()` para descobrir outras maneiras de usar essa função útil.

## Veja Também

- [Documentação da função removeMatchingCharacters()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/removematchingcharacters/) 
- [Tutorial sobre expressões regulares no Arduino](https://create.arduino.cc/projecthub/FablabTutoriais/introducao-a-expressoes-regulares-d4d477)