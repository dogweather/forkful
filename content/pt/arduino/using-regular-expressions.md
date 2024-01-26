---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Expressões regulares (regex) são padrões usados para encontrar correspondências de texto. Elas são úteis para validação de dados, busca, e manipulação de texto de maneira eficiente.

## Como Fazer:
Em Arduino, você usará bibliotecas como `Regexp` para trabalhar com expressões regulares. Aqui está um exemplo:

```Arduino
#include <Regexp.h>

void setup() {
  Serial.begin(9600);

  // Padrao regex: uma letra seguida por dois digitos
  MatchState ms;
  ms.Target ("A23 B56 X99 Y11");

  char result = ms.Match ("[A-Z][0-9]{2}");

  if (result == REGEXP_MATCHED) {
    // Encontrado
    Serial.println("Padrao encontrado");
  } else {
    // Não encontrado
    Serial.println("Padrao nao encontrado");
  }
}

void loop() {
  // Nada aqui
}
```
Resultado esperado:
```
Padrao encontrado
```

## Aprofundando
As expressões regulares têm raízes na teoria da computação e na linguística formal. Enquanto Arduino não tem suporte nativo a regex, bibliotecas como a `Regexp` oferecem funcionalidades limitadas. Alternativas incluem implementar suas próprias funções de busca de padrões ou usar funções simples de string quando adequado.

## Veja Também
- Documentação da biblioteca `Regexp`: http://www.gammon.com.au/Arduino/Regexp.zip
- Tutorial Regex: https://www.regular-expressions.info/tutorial.html
- Artigo sobre expressões regulares na programação: https://www.arduino.cc/reference/en/libraries/regexp/
