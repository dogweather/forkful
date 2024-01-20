---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Por quê?

As expressões regulares são eficientes para procurar, substituir e manipular textos em seu código. Programadores as usam para tornar seu código mais compacto e ordenado.

## Como Fazer:

Para usar uma expressão regular em Arduino, você precisa da biblioteca "regex.h”. Vamos tentar encontrar a palavra "Arduino" numa frase.

```Arduino
#include < regex.h >
char string [] = "Eu amo programar em Arduino!";
char regex [] = "Arduino";

regex_t regexStr;
regcomp(&regexStr, regex, 0);
int status = regexec(&regexStr, string, 0, NULL, 0);
if (status == 0) 
{
  Serial.println("Palavra encontrada.");
} 
else 
{
  Serial.println("Palavra não encontrada.");
}
```
A saída será:
```
Palavra encontrada.
```
## Excursão Profunda:

Historicamente, as expressões regulares começaram com linguagens de programação como Perl. Em Arduino, o suporte a expressões regulares é um pouco limitado devido à sua capacidade reduzida.

Existem alternativas às expressões regulares em Arduino, como funções tradicionais de string como indexOf() e substring(). No entanto, para expressões mais complexas, regex pode ser a melhor opção.

A implementação de regex em Arduino é baseada em bibliotecas integradas. Estas proporcionam funções para compilar e executar expressões regulares.

## Ver Também:

[Usando expressões regulares](https://www.arduino.cc/reference/en/)
[Técnicas de manipulação de strings sem Regex](https://startingelectronics.org/software/arduino/learn-to-program-course/16-strings-char-variables/)
[Documentação Regex](http://www.cplusplus.com/reference/regex/)