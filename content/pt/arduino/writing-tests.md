---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## O Que & Por quê?
Escrever testes é um processo de verificação para garantir que o código escrito funciona corretamente. Os programadores fazem testes para encontrar e corrigir erros no código antes de colocá-lo em produção.

## Como fazer:
Escrever testes no Arduino é bastante simples. Primeiro, é necessário incluir a biblioteca "ArduinoUnit.h". Em seguida, escreva o teste usando a função Test(); e execute-o com a função runalltests();. Por exemplo:

```
#include <ArduinoUnit.h>

void setup()
{
  Serial.begin(9600);
  Test.begin();
}

// Exemplo de teste
test(led_test) 
{
  pinMode(LED_BUILTIN, OUTPUT);
  digitalWrite(LED_BUILTIN, HIGH);
  assertEquals(HIGH, digitalRead(LED_BUILTIN));
}

void loop()
{
  runAllTests();
  while (Test.available()) {
    char c = Test.read();
    Serial.print(c);
  }
}
```

A saída do teste será exibida na porta serial do Arduino. Se todos os testes passarem, a saída será "OK", caso contrário, indicará qual teste falhou e por quê.

## Deep Dive:
O processo de escrever testes é conhecido como Test Driven Development (TDD) e é uma prática comum em desenvolvimento de software. Existem outras bibliotecas de teste disponíveis para o Arduino, como a "Unity" e a "CppUTest".

## Veja também:
- [Documentação da biblioteca ArduinoUnit] (https://github.com/mmurdoch/arduinounit)
- [Artigo sobre TDD no Arduino] (https://www.arduino.cc/en/software-testing-tools)
- [Outras bibliotecas de teste para Arduino] (https://create.arduino.cc/projecthub/?q=testing&t=projects)