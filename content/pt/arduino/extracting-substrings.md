---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extrair Substrings no Arduino: Um Guia Prático

## O Que & Por Quê?

Extrair substrings é o processo de selecionar um conjunto específico de caracteres de uma string. Programadores fazem isso para manipular, comparar ou analisar partes de uma string.

## Como Fazer:

Extrair substrings no Arduino é direto com a função `substring()`. Veja como fazer isso:

```Arduino
String meutexto = "Olá, Mundo!";
String substr = meutexto.substring(0, 4);
Serial.println(substr);
```

O acima vai imprimir:

```Arduino
Olá,
```

A função `substring()` recebe dois argumentos: o índice inicial e o final (exclusivo) da substring.

## Mergulho Profundo

Extrair substrings é um recurso amplamente utilizado na programação desde a sua concepção. No Arduino, além da `substring()`, você também pode extrair substrings usando ponteiros e arrays de char, mas esses métodos são mais complexos e menos eficientes.

A função `substring()` cria uma nova string e copia os caracteres selecionados para ela. Este processo consome memória extra, então, se memória for uma preocupação para você, considere outras opções, como manipulação in-place dos dados.

## Veja Também

Para aprofundar seu conhecimento sobre strings e substrings no Arduino, confira os seguintes links:

1. [Arduino String Reference](https://arduino.cc/reference/en/language/variables/data-types/string)

2. [Arduino String Functions](https://arduino.cc/en/Tutorial/StringFunctions)

3. [Arduino String Constructors](https://arduino.cc/reference/en/language/variables/data-types/string/constructors/)

Lembre-se, a prática é o caminho para aperfeiçoamento, então continue programando e experimentando!