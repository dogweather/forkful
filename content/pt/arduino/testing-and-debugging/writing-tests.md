---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:34.681520-07:00
description: "Escrever testes no ambiente Arduino refere-se ao processo de criar testes\
  \ automatizados que validam a funcionalidade do seu c\xF3digo em dispositivos\u2026"
lastmod: '2024-03-13T22:44:46.842256-06:00'
model: gpt-4-0125-preview
summary: "Escrever testes no ambiente Arduino refere-se ao processo de criar testes\
  \ automatizados que validam a funcionalidade do seu c\xF3digo em dispositivos Arduino."
title: Escrevendo testes
weight: 36
---

## O Que & Por Quê?

Escrever testes no ambiente Arduino refere-se ao processo de criar testes automatizados que validam a funcionalidade do seu código em dispositivos Arduino. Os programadores fazem isso para garantir que seu código funcione conforme esperado, reduza bugs e melhore a qualidade de seus projetos, especialmente crucial em sistemas embarcados onde a depuração pode ser mais desafiadora.

## Como fazer:

O Arduino não possui um framework de teste integrado como alguns outros ambientes de programação. No entanto, você pode usar bibliotecas de terceiros, como o `AUnit`, para testes unitários do código Arduino. AUnit é inspirado pela biblioteca integrada do Arduino, `ArduinoUnit`, e pelo framework de testes do Google, `Google Test`.

### Exemplo com AUnit:

Primeiro, instale o AUnit pelo Gerenciador de Bibliotecas no IDE Arduino: vá em Sketch > Incluir Biblioteca > gerenciar Bibliotecas... > procure por AUnit e instale.

Então, você pode escrever testes assim:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Vazio
}
```
Após enviar este teste para sua placa Arduino, abra o Monitor Serial para ver os resultados dos testes. Você deverá ver uma saída indicando se cada teste passou ou falhou:

```
TestRunner iniciado em 2 teste(s).
Teste ledPinHigh passou.
Teste ledPinLow passou.
Duração do TestRunner: 0.002 segundos.
Resumo do TestRunner: 2 passaram, 0 falharam, 0 pulados, 0 expiraram, de 2 teste(s).
```

Este exemplo simples demonstra o uso do AUnit para testar o estado de um pino de LED. Ao criar testes, você confirma que seu Arduino se comporta conforme esperado em diferentes condições. Com o AUnit, você pode escrever testes mais complexos, suítes de teste e desfrutar de recursos como tempos limite de testes e procedimentos de configuração/encerramento para cenários mais avançados.
