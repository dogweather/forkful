---
date: 2024-01-26 01:16:27.752628-07:00
description: "Refatora\xE7\xE3o \xE9 o processo de reestruturar o seu c\xF3digo para\
  \ melhorar sua estrutura e legibilidade sem alterar o comportamento externo ou funcionalidade.\u2026"
lastmod: '2024-03-13T22:44:46.847042-06:00'
model: gpt-4-0125-preview
summary: "Refatora\xE7\xE3o \xE9 o processo de reestruturar o seu c\xF3digo para melhorar\
  \ sua estrutura e legibilidade sem alterar o comportamento externo ou funcionalidade."
title: "Refatora\xE7\xE3o"
weight: 19
---

## Como:
Vamos dizer que você tem uma função no seu Arduino que está fazendo demais, assim:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Uma função que está fazendo demais
  handleEverything();
}

void handleEverything() {
  // Ler dados do sensor
  int sensorValue = analogRead(A0);
  // Processar os dados do sensor
  sensorValue = map(sensorValue, 0, 1023, 0, 255);
  // Imprimir os dados do sensor
  Serial.println(sensorValue);
  delay(500);
}
```

Refatorá-la pode parecer dividir `handleEverything()` em funções menores e mais focadas:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorValue = readSensorData();
  int processedValue = processSensorData(sensorValue);
  printData(processedValue);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorValue) {
  return map(sensorValue, 0, 1023, 0, 255);
}

void printData(int data) {
  Serial.println(data);
}
```

Após a refatoração, a função `loop()` fica mais legível, e cada tarefa é tratada por uma função dedicada, tornando o código mais fácil de gerenciar.

## Aprofundamento
Historicamente, a refatoração tornou-se popular com o surgimento das metodologias Ágil e Desenvolvimento Orientado por Testes (TDD), que dependem da constante melhoria do código para se adaptar a requisitos em mudança. Existem várias ferramentas e estratégias para refatoração — como a técnica "Extrair Método" que usamos em nosso exemplo com Arduino. Isso é essencial quando você está passando de um protótipo rápido para um projeto estável, onde a legibilidade e manutenção do código se tornam cruciais.

Ao refatorar, é importante ter um bom conjunto de testes para garantir que as mudanças não introduziram bugs. No mundo Arduino, testes automatizados nem sempre são diretos devido às dependências de hardware, mas você ainda pode usar testes unitários para partes puramente lógicas ou empregar simuladores.

Alternativas para refatoração manual incluem usar ferramentas dedicadas à refatoração, que automatizam a identificação de odores de código e sugerem alterações. No entanto, essas ferramentas muitas vezes carecem de nuances para o código de microcontroladores e podem não estar disponíveis no ambiente de desenvolvimento Arduino.

Em última análise, refatoração é uma arte que equilibra a melhoria da estrutura interna do código contra o risco de introduzir defeitos. Requer que você pense sobre detalhes de implementação como uso de memória e tempo de processador, especialmente devido à natureza de recursos limitados dos microcontroladores.

## Veja Também
Você pode se aprofundar mais em refatoração com o livro seminal de Martin Fowler *Refatoração: Melhorando o Design do Código Existente*. Para um olhar mais atento às práticas específicas do Arduino, confira os fóruns e comunidades de desenvolvimento Arduino:

- [Fórum Arduino - Questões de Programação](https://forum.arduino.cc/index.php?board=4.0)
- [Guru da Refatoração](https://refactoring.guru/refactoring)

Lembre-se, o objetivo é um código limpo, compreensível que o futuro você, e outros, agradecerão. Continue hackeando, e mantenha tudo arrumado!
